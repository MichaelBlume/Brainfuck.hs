import sys
import os
try:
    from pypy.rlib.jit import JitDriver
except ImportError:
    class JitDriver(object):
        def __init__(self,**kw): pass
        def jit_merge_point(self,**kw): pass
        def can_enter_jit(self,**kw): pass

jitdriver = JitDriver(greens=['ip', 'program', 'skip_table'],
                      reds=['tape', 'bf'])

class BrainFuck(object):
    def __init__(self, fp):
        self.skip_table = {}
        self.program = []
        self.parse(fp)
        self.tape = Tape()
        self.ip = 0

    def run(self):
        pl = len(self.program)
        while self.ip < pl:
            self.loop_once()

    def loop_once(self):
        instruction = self.program[self.ip]

        if instruction == '<':
            self.tape.retreat()
        elif instruction == '>':
            self.tape.advance()
        elif instruction == '+':
            self.tape.increment()
        elif instruction == '-':
            self.tape.decrement()
        elif instruction == '.':
            os.write(1, chr(self.tape.read()))
        elif instruction == ',':
            self.tape.write(ord(os.read(0,1)[0]))
        elif instruction == ('[' if self.tape.read() == 0 else ']'):
            self.swap_bracket()

        if self.program[self.ip] == '[':
            jitdriver.jit_merge_point(ip=self.ip, program=self.program,
                    skip_table=self.skip_table, tape=self.tape, bf=self)

        self.ip += 1

    def swap_bracket(self):
        self.ip = self.skip_table[self.ip]

    def parse(self, program):
        ip = 0
        left_stack = []
        for c in program:
            if c not in "<>+-.,[]":
                continue
            self.program.append(c)
            if c == '[':
                left_stack.append(ip)
            elif c == ']':
                match = left_stack.pop()
                self.skip_table[match] = ip
                self.skip_table[ip] = match
            ip += 1

class Tape(object):
    right = [0]
    left = []
    address = 0

    def __init__(self):
        pass

    def advance(self):
        self.address += 1
        if self.address == len(self.right):
            self.right.append(0)

    def retreat(self):
        self.address -= 1
        if self.leftind() == len(self.left):
            self.left.append(0)

    def increment(self):
        if self.address >= 0:
            self.right[self.address] += 1
        else:
            self.left[self.leftind()] += 1

    def decrement(self):
        if self.address >= 0:
            self.right[self.address] -= 1
        else:
            self.left[self.leftind()] -= 1

    def read(self):
        if self.address >= 0:
            return self.right[self.address]
        else:
            return self.left[self.leftind()]

    def write(self, val):
        if self.address >= 0:
            self.right[self.address] = val
        else:
            self.left[self.leftind()] = val
    def leftind(self):
        return (-1 - self.address)

def run_brainfuck(fp):
    bf = BrainFuck(fp)
    bf.run()

def target(*args):
    return entry, None

def entry(argv):
    try:
        filename = argv[1]
    except IndexError:
        print "You must supply a filename"
        return 1
    fp = os.open(filename, os.O_RDONLY, 0777)
    program_contents = ""
    while True:
        read = os.read(fp, 4096)
        if len(read) == 0:
            break
        program_contents += read
    run_brainfuck(program_contents)
    return 0

def jitpolicy(driver):
    from pypy.jit.codewriter.policy import JitPolicy
    return JitPolicy()

if __name__ == "__main__":
    entry(sys.argv)
