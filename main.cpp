#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <vector>
#include <queue>
#include <ctime>

using namespace std;

struct Command {
	enum Type { add, sub, mul, div, load, jump } type;

	int op[3];
};

int parse_hex(const char* str, int len = 20) {
	int re = 0, i = 0;
	char s;
	for (; i < len && (s = str[i]) != 0; ++i) {
		if (s == 'x') {
			continue;
		}
		if (s <= '9') {
			s -= '0';
		} else if (s <= 'F') {
			s -= 'A' - 10;
		} else {
			s -= 'a' - 10;
		}
		re = (re << 4) + s;
	}
	return re;
}

int parse_dec(const char* str, int len = 20) {
	int re = 0, i = 0;
	char s;
	for (; i < len && (s = str[i]) != 0; ++i) {
		re = re * 10 + s - '0';
	}
	return re;
}

Command parse_line(const char* str) {
	Command cmd;
	switch (str[0]) {
	case 'A': cmd.type = Command::add;
		break;
	case 'S': cmd.type = Command::sub;
		break;
	case 'M': cmd.type = Command::mul;
		break;
	case 'D': cmd.type = Command::div;
		break;
	case 'L': cmd.type = Command::load;
		break;
	default: cmd.type = Command::jump;
		break;
	}
	int i = 0, start;
	while (str[i++] != ',');
	start = i;
	while (str[++i] != ',');
	if (str[start] == 'R') {
		++start;
		cmd.op[0] = parse_dec(str + start, i - start);
	} else {
		cmd.op[0] = parse_hex(str + start, i - start);
	}
	start = ++i;
	while (str[++i] != ',' && str[i] != 0);
	if (str[i] != 0) {
		if (str[start] == 'R') {
			++start;
			cmd.op[1] = parse_dec(str + start, i - start);
		} else {
			cmd.op[1] = parse_hex(str + start, i - start);
		}
		if (str[++i] == 'R') {
			cmd.op[2] = parse_dec(str + i + 1);
		} else {
			cmd.op[2] = parse_hex(str + i);
		}
	} else {
		if (str[start] == 'R') {
			cmd.op[1] = parse_dec(str + start + 1);
		} else {
			cmd.op[1] = parse_hex(str + start);
		}
		cmd.op[2] = 0;
	}
	return cmd;
}

struct Record {
	// record final result
	int filled; // 0-not filled, 1-filling, 2-filled
	int issue;
	int exec_comp;
	int write_result;
};

class Tomasulo {
	struct RS {
		// 保留站
		bool busy;
		char op; // 0-加，1-减，2-乘，3-除，4-jump
		int vj, vk; // 源操作数的值
		int qj, qk; // 产生源操作数的RS，为0表示操作数已经就绪或者不需要
	};

	struct LB {
		// Load Buffer
		bool busy;
		int addr; // 地址
	};

	struct FU {
		int id; // 指令id+1
		int count; // 剩余cycle
	};

	class Inst {
	public:
		enum State { issued, ready, executing } state;

		int id;
		int rs_id;
		int fu_id;
		Inst* next;

		Inst(int id_, int rs_, int fu_ = -1) : state(issued), id(id_), rs_id(rs_), fu_id(fu_), next(nullptr) {
		}
	};

	class InstManager {
		Inst* rubbish = nullptr;
	public:
		Inst* create(int id_, int rs_) {
			Inst* p;
			if (rubbish == nullptr) {
				p = new Inst(id_, rs_);
			} else {
				p = rubbish;
				rubbish = p->next;
				p->state = Inst::issued;
				p->id = id_;
				p->rs_id = rs_;
				//p->fu_id = -1;
				p->next = nullptr;
			}
			return p;
		}

		void del(Inst* p) {
			p->next = rubbish;
			rubbish = p;
		}
	};

	struct Item {
		int id, count;
		Inst* p;

		Item(int id_, int count_, Inst* p_): id(id_), count(count_), p(p_) {
		}
	};

	RS* rs; // 保留站
	LB* lb; // load buffer
	FU* fu; // function unit
	int *reg_state, *reg_state_; // 寄存器状态，0表示就绪
	int reg[32]; // 寄存器值
	int pc; // PC

	void copy() {
		// copy states
		memcpy(reg_state_, reg_state, 32 * sizeof(int));
	}

	void sync() {
		// sync changes
		auto t = reg_state;
		reg_state = reg_state_;
		reg_state_ = t;
	}

public:
	Tomasulo(): pc(0) {
		rs = new RS[9];
		lb = new LB[3];
		fu = new FU[7];
		reg_state = new int[32];
		reg_state_ = new int[32];
		memset(rs, 0, 9 * sizeof(RS));
		memset(lb, 0, 3 * sizeof(LB));
		memset(fu, 0, 7 * sizeof(FU));
		memset(reg_state, 0, 32 * sizeof(int));
		memset(reg_state_, 0, 32 * sizeof(int));
		memset(reg, 0, 32 * sizeof(int));
	}

	Record* run(vector<string>& lines, bool log = true) {
		pc = 0;
		int size = lines.size();
		auto cmds = new Command[size];
		auto records = new Record[size];
		memset(records, 0, size * sizeof(Record));
		for (int i = 0; i < size; ++i) {
			cmds[i] = parse_line(lines[i].c_str());
		}
		if (!log) {
			lines.clear();
		}
		Inst *head = nullptr, *tail = nullptr;
		// 由于不做分支预测，可以不考虑多重jump的情况，用一套变量存jump状态即可
		bool jumping = false; // 是否正在等待跳转
		int jump_val; // jump判等值
		int jump_pc; // jump目标pc
		int cycle = 0;
		int i;
		bool found;
		Inst *p, *last_p;
		InstManager IM;
		queue<Item> fu_q[3];
		while (true) {
			copy();
			++cycle;

			// execute commands
			p = head;
			last_p = nullptr;
			while (p != nullptr) {
				if (p->state == Inst::executing) {
					auto& cmd = cmds[p->id];
					auto& r = rs[p->rs_id];
					auto f = fu[p->fu_id];
					if (f.count == 0) {
						// calc result
						int result;
						switch (cmd.type) {
						case Command::add:
							result = r.vj + r.vk;
							break;
						case Command::sub:
							result = r.vj - r.vk;
							break;
						case Command::mul:
							result = r.vj * r.vk;
							break;
						case Command::div:
							result = r.vj / (r.vk == 0 ? 1 : r.vk);
							break;
						case Command::load:
							result = lb[p->rs_id].addr;
							break;
						case Command::jump:
							break;
						}

						auto id = p->rs_id + 1;
						if (cmd.type == Command::load) {
							id += 9;
						}
						if (cmd.type != Command::jump) {
							// transfer data
							// to RS
							for (i = 0; i < 9; ++i) {
								if (rs[i].qj == id) {
									rs[i].vj = result;
									rs[i].qj = 0;
								}
								if (rs[i].qk == id) {
									rs[i].vk = result;
									rs[i].qk = 0;
								}
							}

							// to reg (there's no WAR)
							for (i = 0; i < 32; ++i) {
								if (reg_state[i] == id) {
									// 执行状态的指令rs_id必然不同，不用担心WAW
									reg[i] = result;
									reg_state[i] = reg_state_[i] = 0; // 下个周期issue的指令也要接收到改变
								}
							}
						} else {
							// jump
							if (jump_val == r.vj) {
								pc = jump_pc;
							}
							jumping = false;
						}

						// release LB & RS &FU
						if (cmd.type == Command::load) {
							lb[p->rs_id].busy = false;
						} else {
							rs[p->rs_id].busy = false;
						}
						fu[p->fu_id].id = 0;

						// record cycle
						auto& rec = records[p->id];
						if (rec.filled == 1) {
							rec.exec_comp = cycle - 1;
							rec.write_result = cycle;
							rec.filled = 2;
						}

						// remove self
						if (p == tail) {
							tail = last_p;
						}
						auto pn = p->next;
						if (p == head) {
							head = pn;
						} else {
							last_p->next = pn;
						}
						IM.del(p);
						p = pn;
						continue;
					}
				}
				last_p = p;
				p = p->next;
			}

			// for all FU, count--
			for (i = 0; i < 7; ++i) {
				--fu[i].count;
			}

			// fetch cmd
			if (!jumping && pc < size) {
				auto& cmd = cmds[pc];

				// check if new cmd can be issued
				bool issued = false;
				switch (cmd.type) {
				case Command::add:
				case Command::sub:
					found = false;
					for (i = 0; i < 6; ++i) {
						// check rs
						if (!rs[i].busy) {
							found = true;
							break;
						}
					}
					if (found) {
						// rs available, issue
						issued = true;

						// modify rs table
						auto& r = rs[i];
						r.busy = true;
						r.op = (cmd.type == Command::add) ? 0 : 1;
						if (reg_state[cmd.op[1]] == 0) {
							// check if reg available
							r.vj = reg[cmd.op[1]];
							r.qj = 0;
						} else {
							r.qj = reg_state[cmd.op[1]];
						}
						if (reg_state[cmd.op[2]] == 0) {
							// check if reg available
							r.vk = reg[cmd.op[2]];
							r.qk = 0;
						} else {
							r.qk = reg_state[cmd.op[2]];
						}

						// modify reg state table
						reg_state_[cmd.op[0]] = i + 1;
					}
					break;
				case Command::mul:
				case Command::div:
					found = false;
					for (i = 6; i < 9; ++i) {
						// check rs
						if (!rs[i].busy) {
							found = true;
							break;
						}
					}
					if (found) {
						// rs available, issue
						issued = true;

						// modify rs table
						auto& r = rs[i];
						r.busy = true;
						r.op = (cmd.type == Command::mul) ? 2 : 3;
						if (reg_state[cmd.op[1]] == 0) {
							// check if reg available
							r.vj = reg[cmd.op[1]];
							r.qj = 0;
						} else {
							r.qj = reg_state[cmd.op[1]];
						}
						if (reg_state[cmd.op[2]] == 0) {
							// check if reg available
							r.vk = reg[cmd.op[2]];
							r.qk = 0;
						} else {
							r.qk = reg_state[cmd.op[2]];
						}

						// modify reg state table
						reg_state_[cmd.op[0]] = i + 1;
					}
					break;
				case Command::load:
					found = false;
					for (i = 0; i < 3; ++i) {
						// check LB
						if (!lb[i].busy) {
							found = true;
							break;
						}
					}
					if (found) {
						// LB available, issue
						issued = true;

						// modify LB table
						auto& l = lb[i];
						l.busy = true;
						l.addr = cmd.op[1];

						// modify reg state table
						reg_state_[cmd.op[0]] = i + 10;
					}
					break;
				case Command::jump:
					found = false;
					for (i = 0; i < 6; ++i) {
						// check rs
						if (!rs[i].busy) {
							found = true;
							break;
						}
					}
					if (found) {
						// rs available, issue
						issued = true;

						// store jump info
						jumping = true;
						jump_pc = cmd.op[2] + pc;
						jump_val = cmd.op[0];

						// modify rs table
						auto& r = rs[i];
						r.busy = true;
						r.op = 4;
						if (reg_state[cmd.op[1]] == 0) {
							// check if reg available
							r.vj = reg[cmd.op[1]];
							r.qj = 0;
						} else {
							r.qj = reg_state[cmd.op[1]];
						}
					}
				}
				if (issued) {
					// append cmd
					auto h = IM.create(pc, i);
					if (head == nullptr) {
						head = tail = h;
					} else {
						tail = tail->next = h;
					}

					// record issue cycle
					auto& rec = records[pc];
					if (rec.filled == 0) {
						rec.issue = cycle;
						rec.filled = 1;
					}

					// add pc
					pc += 1;
				}
			}

			// check if issued commands are ready
			p = head;
			while (p != nullptr) {
				if (p->state == Inst::issued) {
					auto& cmd = cmds[p->id];
					auto& r = rs[p->rs_id];
					// check Qj, Qk
					if (cmd.type == Command::load || (r.qj == 0 && r.qk == 0)) {
						p->state = Inst::ready;
						auto div_count = 4;
						switch (cmd.type) {
						case Command::add:
						case Command::sub:
						case Command::jump:
							fu_q[0].emplace(p->id + 1, (cmd.type == Command::jump) ? 1 : 3, p);
							break;
						case Command::div:
							if (r.vk == 0) {
								div_count = 1; // 除以0只要1周期
							}
						case Command::mul:
							fu_q[1].emplace(p->id + 1, div_count, p);
							break;
						case Command::load:
							fu_q[2].emplace(p->id + 1, 3, p);
							break;
						}
					}
				}
				p = p->next;
			}
			int lut[] = {0, 0, 0, 1, 1, 2, 2};
			for (i = 0; i < 7; ++i) {
				auto j = lut[i];
				if (fu[i].id == 0 && !fu_q[j].empty()) {
					auto t = fu_q[j].front();
					fu_q[j].pop();
					fu[i].id = t.id;
					fu[i].count = t.count;
					t.p->state = Inst::executing;
					t.p->fu_id = i;
				}
			}

			sync();
			if (log) {
				show(lines, cycle);
			}
			if (head == nullptr) {
				break;
			}
			//system("pause");
		}
		return records;
	}

	void show(vector<string>& lines, int cycle) {
		const char* rsm[] = {"", "Busy", "Op", "Vj", "Vk", "Qj", "Qk"};
		const char* rsn[] = {
			"", "[Ars 1]", "[Ars 2]", "[Ars 3]", "[Ars 4]", "[Ars 5]", "[Ars 6]", "[Mrs 1]", "[Mrs 2]", "[Mrs 3]",
			"[LB 1]", "[LB 2]", "[LB 3]"
		};
		const char* lbn[] = {"[LB 1]", "[LB 2]", "[LB 3]"};
		const char* fun[] = {
			"[Add 1]", "[Add 2]", "[Add 3]", "[Mult 1]", "[Mult 2]", "[Load 1]", "[Load 2]"
		};
		const char* op[] = {"ADD", "SUB", "MUL", "DIV", "JUMP"};
		int w = 12;
		cout << "\n===========================================================\n";
		cout << "Cycle: " << cycle << endl;
		cout << "保留站状态：\n";
		cout.setf(ios::left);
		cout << hex;
		for (auto& i : rsm) {
			cout << setw(w) << i;
		}
		cout << endl;
		for (int i = 0; i < 9; ++i) {
			cout << setw(w) << rsn[i + 1];
			if (rs[i].busy) {
				cout << setw(w) << "Yes";
				cout << setw(w) << op[rs[i].op];
				if (rs[i].qj == 0) {
					cout << "0x" << setw(w - 2) << rs[i].vj;
				} else {
					cout << setw(w) << "";
				}
				if (rs[i].qk == 0 && rs[i].op != 4) {
					cout << "0x" << setw(w - 2) << rs[i].vk;
				} else {
					cout << setw(w) << "";
				}
				cout << setw(w) << rsn[rs[i].qj];
				cout << setw(w) << rsn[rs[i].qk];
			} else {
				cout << setw(w) << "No";
			}
			cout << endl;
		}
		cout << "\nLoad Buffer 状态：\n";
		cout << setw(w) << "" << setw(w) << "Busy" << setw(w) << "Address" << endl;
		for (int i = 0; i < 3; ++i) {
			cout << setw(w) << lbn[i];
			if (lb[i].busy) {
				cout << setw(w) << "Yes" << "0x" << setw(w - 2) << (unsigned int)lb[i].addr << endl;
			} else {
				cout << setw(w) << "No" << endl;
			}
		}
		cout << "\n寄存器状态：\n";
		cout << dec;
		cout << setw(w) << "R";
		for (int i = 0; i < 8; ++i) {
			cout << "+" << setw(w - 1) << i;
		}
		cout << endl;
		for (int i = 0; i < 4; ++i) {
			auto ii = i << 3;
			cout << "+" << setw(w - 1) << ii;
			for (int j = 0; j < 8; ++j) {
				auto ind = ii + j;
				if (reg_state[ind] == 0) {
					cout << setw(w) << reg[ind];
				} else {
					cout << setw(w) << rsn[reg_state[ind]];
				}
			}
			cout << endl;
		}
		cout << "\n运算部件状态：\n";
		int w2 = 30;
		cout << setw(w) << "" << setw(w2) << "当前执行指令" << "剩余周期数\n";
		for (int i = 0; i < 7; ++i) {
			auto& id = fu[i].id;
			if (id == 0) {
				cout << setw(w) << fun[i] << endl;
			} else {
				cout << setw(w) << fun[i] << setw(w2) << lines[fu[i].id - 1] << fu[i].count << endl;
			}
		}
		cout << endl;
	}
};


int main(int argc, const char* argv[]) {
	auto t1 = clock();
#if false
	argc = 4;
	const char* cmd[] = {"", "C:\\Users\\johna\\Data\\Big_test.nel", "C:\\Users\\johna\\Data\\Example.log"};
	argv = cmd;
#endif
	if (argc < 3) {
		cout << "Usage: tomasulo.exe input output [-q]\n\n";
		cout << "    input\tpath to NEL file\n";
		cout << "    output\tpath to log file\n";
		cout << "    -q\t\tQuiet mode, do not print state info to stdout except time\n\n";
		return 0;
	}
	bool quiet = (argc > 3);
	ifstream input;
	ofstream output;
	input.open(argv[1]);
	if (!input.is_open()) {
		cout << "Failed to open input file!\n";
		return 0;
	}
	output.open(argv[2]);
	if (!output.is_open()) {
		cout << "Failed to open output file!\n";
		return 0;
	}
	vector<string> lines;
	string line;
	while (!input.eof()) {
		input >> line;
		lines.push_back(line);
	}
	input.close();
	Tomasulo tomasulo;
	int n = lines.size();
	auto t2 = clock();
	auto records = tomasulo.run(lines, !quiet);
	t2 = clock() - t2;
	for (int i = 0; i < n; ++i) {
		auto& r = records[i];
		output << r.issue << ' ' << r.exec_comp << ' ' << r.write_result << '\n';
	}
	output.close();
	t1 = clock() - t1;
	cout << "Total Time: " << t1 << "ms, Simulation Time: " << t2 << "ms\n";
	//system("pause");
	return 0;
}
