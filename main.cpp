#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace std;

struct Command {
	enum Type { add, sub, mul, div, load, jump } type;

	int op[3];
};

int parse_hex(const char* str, int len = 20) {
	int re = 0, i = 0;
	char s;
	for (int i = 0; i < len && (s = str[i]) != 0; ++i) {
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
	for (int i = 0; i < len && (s = str[i]) != 0; ++i) {
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

class Tomasulo {
	struct RS {
		// 保留站
		bool busy;
		char op; // 0表示加/乘，1表示减/除，2表示jump
		int vj, vk; // 源操作数的值
		int qj, qk; // 产生源操作数的RS，为0表示操作数已经就绪或者不需要
	};

	struct LB {
		// Load Buffer
		bool busy;
		int addr; // 地址
	};

	struct FU {
		int id; // 指令id
		int count; // 剩余cycle
	};

	class Inst {
	public:
		enum State { pending, issued, executing, executed, written } state;

		int id;
		int rs_id;
		int fu_id;
		Inst* next;

		Inst(int id_, int rs_, int fu_ = -1) : state(issued), id(id_), rs_id(rs_), fu_id(fu_), next(nullptr) {
		}
	};

	struct Record {
		// record final result
		int filled; // 0-not filled, 1-filling, 2-filled
		int id;
		int issue;
		int complete;
	};

	RS *rs, *rs_; // 保留站
	LB *lb, *lb_; // load buffer
	FU *fu, *fu_; // function unit
	int *reg_state, *reg_state_; // 寄存器状态，-1表示就绪
	int reg[32]; // 寄存器值
	int pc; // PC

	void copy() {
		// copy states
		memcpy(rs_, rs, 9 * sizeof(RS));
		memcpy(lb_, lb, 3 * sizeof(LB));
		memcpy(fu_, fu, 3 * sizeof(FU));
		memcpy(reg_state_, reg_state, 32 * sizeof(bool));
	}

	void sync() {
		// sync changes
		void* t = rs;
		rs = rs_;
		rs_ = (RS*)t;
		t = lb;
		lb = lb_;
		lb_ = (LB*)t;
		t = fu;
		fu = fu_;
		fu_ = (FU*)t;
		t = reg_state;
		reg_state = reg_state_;
		reg_state_ = (int*)t;
		//memcpy(rs, rs_, 9 * sizeof(RS));
		//memcpy(lb, lb_, 3 * sizeof(LB));
		//memcpy(reg_state, reg_state_, 32 * sizeof(bool));
	}

public:
	Tomasulo(): pc(0) {
		rs = new RS[9];
		rs_ = new RS[9];
		lb = new LB[3];
		lb_ = new LB[3];
		fu = new FU[7];
		fu_ = new FU[7];
		reg_state = new int[32];
		reg_state_ = new int[32];
		memset(rs, 0, 9 * sizeof(RS));
		memset(rs_, 0, 9 * sizeof(RS));
		memset(lb, 0, 3 * sizeof(LB));
		memset(lb_, 0, 3 * sizeof(LB));
		memset(fu, 0, 7 * sizeof(FU));
		memset(fu_, 0, 7 * sizeof(FU));
		memset(reg_state, 0, 32 * sizeof(int));
		memset(reg_state_, 0, 32 * sizeof(int));
		memset(reg, 0, 32 * sizeof(int));
	}

	void run(vector<string>& lines, bool log = true) {
		pc = 0;
		int size = lines.size();
		auto cmds = new Command[size];
		auto records = new Record[size];
		memset(records, 0, size * sizeof(Record));
		for (int i = 0; i < size; ++i) {
			cmds[i] = parse_line(lines[i].c_str());
		}
		//auto head = new Inst;
		//head->id = 0;
		//head->state = Inst::issued;
		//head->next = nullptr;
		Inst *head = nullptr, *tail = nullptr;
		bool jumping = false; // 是否正在等待跳转
		int jump_val; // jump判等值
		int jump_pc; // jump目标pc (由于不做分支预测，可以不考虑多重jump的情况)
		while (pc < size) {
			auto& cmd = cmds[pc];
			char addmul = 0;
			bool found;
			int i;

			copy();

			// for all FU, count--
			for (i = 0; i < 7; ++i) {
				if (fu_[i].count > 0) {
					fu_[i].count -= 1;
				}
			}

			// check if new cmd can be issued
			if (!jumping) {
				switch (cmd.type) {
				case Command::sub:
					addmul = 1;
				case Command::add:
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
						if (tail == nullptr) {
							head = tail = new Inst(pc, i);
						} else {
							tail->next = new Inst(pc, i);
							tail = tail->next;
						}

						// add pc
						pc += 1;

						// modify rs table
						auto& r = rs_[i];
						r.busy = true;
						r.op = addmul;
						if (reg_state[cmd.op[1]] < 0) {
							// check if reg available
							r.vj = reg[cmd.op[1]];
							r.qj = 0;
						} else {
							r.qj = reg_state[cmd.op[1]];
						}
						if (reg_state[cmd.op[2]] < 0) {
							// check if reg available
							r.vj = reg[cmd.op[2]];
							r.qj = 0;
						} else {
							r.qj = reg_state[cmd.op[2]];
						}

						// modify reg state table
						reg_state_[cmd.op[0]] = i;

						//// check if FU available
						//found = false;
						//for (i = 0; i < 3; ++i) {
						//	if (fu[i].count == 0) {
						//		found = true;
						//		break;
						//	}
						//}
						//if (found) {
						//	// fu available, execute
						//	fu_[i].id = pc;
						//	fu_[i].count = 3;
						//	tail->state = Inst::executing;
						//}
					}
					break;
				case Command::div:
					addmul = 1;
				case Command::mul:
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
						if (tail == nullptr) {
							head = tail = new Inst(pc, i);
						} else {
							tail->next = new Inst(pc, i);
							tail = tail->next;
						}

						// add pc
						pc += 1;

						// modify rs table
						auto& r = rs_[i];
						r.busy = true;
						r.op = addmul;
						if (reg_state[cmd.op[1]] < 0) {
							// check if reg available
							r.vj = reg[cmd.op[1]];
							r.qj = 0;
						} else {
							r.qj = reg_state[cmd.op[1]];
						}
						if (reg_state[cmd.op[2]] < 0) {
							// check if reg available
							r.vj = reg[cmd.op[2]];
							r.qj = 0;
						} else {
							r.qj = reg_state[cmd.op[2]];
						}

						// modify reg state table
						reg_state_[cmd.op[0]] = i;

						//// check if FU available
						//found = false;
						//for (i = 3; i < 5; ++i) {
						//	if (fu[i].count == 0) {
						//		found = true;
						//		break;
						//	}
						//}
						//if (found) {
						//	// fu available, execute
						//	fu_[i].id = pc;
						//	fu_[i].count = 4;
						//	tail->state = Inst::executing;
						//}
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
						if (tail == nullptr) {
							head = tail = new Inst(pc, i);
						} else {
							tail->next = new Inst(pc, i);
							tail = tail->next;
						}

						// add pc
						pc += 1;

						// modify LB table
						auto& l = lb_[i];
						l.busy = true;
						l.addr = cmd.op[1];

						// modify reg state table
						reg_state_[cmd.op[0]] = i;

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
						if (tail == nullptr) {
							head = tail = new Inst(pc, i);
						} else {
							tail->next = new Inst(pc, i);
							tail = tail->next;
						}

						// store jump info
						jumping = true;
						jump_pc = cmd.op[2];
						jump_val = cmd.op[0];

						// modify rs table
						auto& r = rs_[i];
						r.busy = true;
						r.op = 2;
						if (reg_state[cmd.op[1]] < 0) {
							// check if reg available
							r.vj = reg[cmd.op[1]];
							r.qj = 0;
						} else {
							r.qj = reg_state[cmd.op[1]];
						}

						// modify reg state table
						reg_state_[cmd.op[0]] = i;
					}
				}
			}

			// execute cmds
			auto p = head;
			while (p != nullptr) {
				auto cmd = cmds[p->id];
				auto r = rs[p->rs_id];
				switch (p->state) {
				case Inst::issued:
					// check Qj, Qk
					if (cmd.type == Command::load || (r.qj == 0 && r.qk == 0)) {
						// check FU
						auto div_count = 4;
						switch (cmd.type) {
						case Command::add:
						case Command::sub:
						case Command::jump:
							found = false;
							for (i = 0; i < 3; ++i) {
								if (fu[i].count == 0) {
									found = true;
									break;
								}
							}
							if (found) {
								// fu available, execute
								fu_[i].id = p->id;
								fu_[i].count = 3;
								p->state = Inst::executing;
								p->fu_id = i;
							}
							break;
						case Command::div:
							if (r.vk == 0) {
								div_count = 1; // 除以0只要1周期
							}
						case Command::mul:
							found = false;
							for (i = 4; i < 6; ++i) {
								if (fu[i].count == 0) {
									found = true;
									break;
								}
							}
							if (found) {
								// fu available, execute
								fu_[i].id = p->id;
								fu_[i].count = div_count;
								p->state = Inst::executing;
								p->fu_id = i;
							}
							break;
						case Command::load:
							found = false;
							for (i = 6; i < 8; ++i) {
								if (fu[i].count == 0) {
									found = true;
									break;
								}
							}
							if (found) {
								// fu available, execute
								fu_[i].id = p->id;
								fu_[i].count = 3;
								p->state = Inst::executing;
								p->fu_id = i;
							}
							break;
						}
					}
					break;
				case Inst::executing:
					auto f = fu[p->fu_id];
					if (f.count == 0) {
						// TODO: 命令执行完成，写回结果、维护数据
					}
				}
			}
		}
	}

};


int main(int argc, const char* argv[]) {
#if true
	argc = 3;
	const char* cmd[] = {"tomasulo.exe", "C:\\Users\\johna\\Data\\0.basic.nel", "C:\\Users\\johna\\Data\\0.basic.log"};
	argv = cmd;
#endif
	if (argc < 3) {
		cout << "Usage: tomasulo.exe input output [-q]\n\n";
		cout << "    input\tpath to NEL file\n";
		cout << "    output\tpath to log file\n";
		cout << "    -q\tQuiet mode, do not print state info to stdout\n\n";
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
		//auto c = parse_line(line.c_str());
		//cout << c.type << " " << c.op[0] << " " << c.op[1] << " " << c.op[2] << endl;
		lines.push_back(line);
	}
	Tomasulo tomasulo;
	tomasulo.run(lines, !quiet);

	system("pause");
	return 0;
}
