#!/usr/bin/env ruby
require 'strscan'
require 'matrix'

module MiniPS
  PRODUCT_NAME = "miniPS"
  VERSION      = "0010"
  DEFAULT_BBOX = "0 0 595 842"

  FONTMAP = {
    "Times-Roman" => "Times New Roman",
    "Courier" => "Courier New"
  }

  # utilities
  def self.private_module_function(name)
    module_function name
    private_class_method name
  end

  module Util
    def deg2rad(d)
      Math::PI * d / 180
    end
    module_function :deg2rad

    def rad2deg(r)
      180 * r / Math::PI
    end
    module_function :rad2deg

    def color_to_hex(c)
      r = (c[0] * 255).to_i
      g = (c[1] * 255).to_i
      b = (c[2] * 255).to_i
      sprintf("%06X", r << 16 | g << 8 | b)
    end
    module_function :color_to_hex
  end
  extend Util

  class Scanner < StringScanner
    def initialize(str)
      super str
      @lfpos = [-1]
      while true
        i = str.index("\n", @lfpos[-1]+1)
        if i.nil?
          break
        end
        @lfpos << i
      end
      @lfpos << str.size
    end

    def peekc
      c = peek(1)
      c == "" ? nil : c
    end

    def readc
      c = getch
      c == "" ? nil : c
    end

    def location
      i = pos
      l = @lfpos.index{|j| j >= i}
      {line: l, column: i - @lfpos[l - 1] - 1}
    end
  end

  module Parser
    RE_DELIM = /(?=\s|[\/\(\)\[\]{}<>]|\Z)/

    class EofError < SyntaxError
      def initialize(msg="unexpected EOF")
        super
      end
    end

    def self.read_num_or_name(s)
      pos = s.location
      e = s.search_full(RE_DELIM, true, true)
      if e.nil?
        e = s.rest
      end
      case e
      when /^-?[0-9]+$/
        {type: :numeric, val: e.to_i, pos: pos}
      when /^-?[0-9]+(\.[0-9]+)?(e-?[0-9]+)?$/
        {type: :numeric, val: e.to_f, pos: pos}
      when /^([2-9]|[1-2][0-9]|3[0-6])#([0-9A-Za-z]+)$/
        {type: :numeric, val: $2.to_i($1.to_i), pos: pos}
      else
        {type: :executable_name, val: e, pos: pos}
      end
    end

    def self.read_literal_name(s)
      pos = s.location
      s.readc # skip "/"
      e = s.search_full(RE_DELIM, true, true)
      if e.nil?
        e = s.rest
      end
      {type: :literal_name, val: e, pos: pos}
    end

    def self.read_str(s)
      buf = ""
      pos = s.location
      s.readc
      until s.peekc == ")"
        c = s.peekc || raise(EofError)
        case c
        when "\\"
          s.readc
          c = s.readc
          case c
          when /[0-7]/     #charcode in octal
            n = c.to_i
            2.times {|i|
              if s.peekc =~ /[0-7]/
                n = n * 8 + (s.readc.to_i)
              else
                break
              end
            }
            buf << n.chr
          when /[bfnrt]/
            buf << {
              "b" => "\b",
              "f" => "\f",
              "n" => "\n",
              "r" => "\r",
              "t" => "\t"
            }[c]
          when "\n"
            # skip
          else
            buf << c
          end
        when "("  # nested ( )
          buf << "(" + read_str(s)[:val] + ")"
        else
          buf << s.readc
        end
      end
      s.readc # skip
      {type: :string, val: buf, pos: pos}
    end

    def self.read_one(s)
      s.skip(/\s+/)
      return nil if s.eos?

      c = s.peekc
      pos = s.location
      case c
      when /\s/
        s.readc # noop
        read_one(s)
      when "%"
        com = s.search_full(/\n|\Z/, true, true).chomp!
        {type: :comment, val: com, pos: pos}
      when /[0-9-]/
        read_num_or_name(s)
      when /[A-Za-z_]/
        read_num_or_name(s)
      when /[!\"$&\'*+,\-.:;=?@\\^`|]/
        read_num_or_name(s)
      when /\//
        read_literal_name(s)
      when /\(/
        read_str(s)
      when "["
        s.readc # skip
        {type: :executable_name, val: "[", pos: pos}
      when "]"
        s.readc #skip
        {type: :executable_name, val: "]", pos: pos}
      when "{"
        s.readc # skip
        arr = []
        while true
          s.skip(/\s+/)
          if s.eos?
            raise EofError
          elsif s.peekc == "}"
            s.readc # skip
            break
          end
          arr << read_one(s)
        end
        {type: :executable_array, val: arr, pos: pos}
      else
        raise SyntaxError, "unknown char '#{c}' at " + pos.to_s
      end
    end

    def self.parse_string(src)
      s = Scanner.new(src)
      prog = []
      while true
        e = read_one(s)
        unless e
          break
        end
        prog << e
      end
      prog
    end

    def self.expr_to_s(e)
      if e[:val].is_a?(Array)
        e[:type].to_s + ":[" + e[:val].map{|e| expr_to_s e} * ", " + "]"
      else
        e[:type].to_s + ":" + e[:val].to_s
      end
    end
    
    def self.readfile(f)
      File.open(f, "r:ascii") do |fi|
        parse_string(fi.read)
      end
    end
  end

  class Value
    attr_reader :type, :value

    def initialize(t, v)
      @type = t
      @value = v
    end

    def to_s
      case @type
      when :literal_name, :bool
        @value.to_s
      else
        "--nostringval--"
      end
    end

    def to_readable
      case @type
      when :literal_name
        "/" + @value
      when :array
        "[" + (@value.map(&:to_readable) * " ") + "]"
      when :mark
        "-mark-"
      when :dict
        "-dict-"
      else
        @value.to_s
      end
    end

    def eq(v)
      self.equal?(v)
    end

    def cmp(v)
      raise TypeError, to_readable + " is not a number nor a string"
    end

    def ensure_value_type(t)
      if @value.is_a? t
        self
      else
        raise TypeError, "#{to_readable} is not a #{t}"
      end
    end
  end

  class Num < Value
    def initialize(n)
      super :numeric, n
    end

    def to_s
      @value.to_s
    end

    def eq(v)
      v.is_a?(Num) && @value == v.value
    end
    
    def cmp(v)
      if !v.is_a?(Num)
        raise TypeError, "#{v.to_readable} is not a number"
      end
      @value <=> v.value
    end
  end

  class Str < Value
    attr_reader :cap, :from, :len
    def initialize(buf)
      super :string, buf
      @cap = buf.length
      @from = 0
      @len = buf.length
    end

    def to_s
      @value[@from, @len]
    end

    def to_readable
      "(" + to_s.each_char.map{|c|
        case c
        when /[\b\f\n\r\t]/
          {"\b" => "\\b", "\f" => "\\f", "\n" => "\\n", "\r" => "\\r", "\t" => "\\t"}[c]
        when /[()]/
          "\\" + c
        else
          if c.ord < 31 || c.ord >= 127
            sprintf("\\%03o", c.ord)
          else
            c
          end
        end
      }.join("") + ")"
    end

    def slice!(from, len)
      if @from + from + len > @cap
        raise "range error"
      end
      @from += from
      @len = len
      self
    end

    def slice(from, len)
      self.dup.slice!(from, len)  # shares @value
    end

    def write(from, s)
      if from + s.length > @len
        raise "range error"
      end
      @value[from, s.length] = s
      s.length
    end

    def eq(v)
      v.is_a?(Str) && v.to_s == self.to_s
    end

    def cmp(v)
      if !v.is_a?(Str)
        raise TypeError, "#{v.to_readable} is not a string"
      end
      self.to_s <=> v.to_s
    end
  end

  class Operator < Value
    def initialize(name, &proc)
      @name = name
      super :operator, proc
    end

    def [](vm)
      @value[vm]
      true
    end

    def to_readable
      "--#{@name}--"
    end
  end

  class Procedure < Value
    def initialize(execarr)
      super :procedure, execarr
    end

    def [](vm)
      vm.eval_prog(@value)
    end

    def to_readable
      "{" + (@value.map{|tk| tk[:val].to_s} * " ") + "}"
    end
  end

  VNULL = Value.new(:null, "null").freeze
  VTRUE = Value.new(:bool, true).freeze
  VFALSE = Value.new(:bool, false).freeze
  VMARK = Value.new(:mark, "-mark-").freeze

  SYSTEM_DICT = Value.new(:dict, {
    "null" => VNULL,
    "true" => VTRUE,
    "false" => VFALSE,
    "[" => VMARK
  }).freeze

  def define_op(name, &proc)
    SYSTEM_DICT.value[name] = Operator.new(name, &proc)
  end
  module_function :define_op

  # stack operators
  define_op("pstack"){|vm|
    vm.ostack.reverse_each {|v|
      vm.stdout.puts(v.to_readable)
    }
  }
  define_op("stack"){|vm|
    vm.ostack.reverse_each {|v|
      vm.stdout.puts(v.to_s)
    }
  }
  define_op("pop"){|vm| vm.pop_op}
  define_op("clear"){|vm| vm.ostack.clear}
  define_op("count"){|vm| vm.push_op(Num.new(vm.ostack.size))}
  define_op("dup"){|vm|
    v = vm.pop_op
    vm.push_op(v)
    vm.push_op(v)
  }
  define_op("index"){|vm|
    i = vm.pop_op().ensure_value_type(Integer)
    if vm.ostack.size <= i.value
      raise "stack underflow"
    end
    vm.push_op(vm.ostack[-1 - i.value])
  }
  define_op("copy"){|vm|
    i = vm.pop_op(:numeric).value
    j = Num.new(i - 1)
    idx = SYSTEM_DICT.value["index"]
    i.times {|_|
      vm.push_op(j)
      idx[vm]
    }
  }
  define_op("exch"){|vm|
    a = vm.pop_op
    b = vm.pop_op
    vm.push_op(a)
    vm.push_op(b)
  }
  define_op("roll"){|vm|
    j = vm.pop_op.ensure_value_type(Integer).value
    n = vm.pop_op.ensure_value_type(Integer).value
    if n > vm.ostack.size
      raise "range error"
    end
    a = vm.ostack[-n .. -1].rotate(-j)
    a.each_with_index{|v, i| vm.ostack[-n + i] = v }
  }
  define_op("="){|vm|
    v = vm.pop_op
    vm.stdout.puts(v.to_s)
  }
  define_op("=="){|vm|
    v = vm.pop_op
    vm.stdout.puts(v.to_readable)
  }

  # mark operators
  define_op("]"){|vm|
    vs = vm.pop_to_mark
    vm.push_op(Value.new(:array, vs))
  }
  define_op("mark"){|vm| vm.mark }
  define_op("counttomark"){|vm|
    ri = vm.ostack.reverse.index{|v| v.type == :mark}
    if ri.nil?
      raise "unmatched mark"
    else
      vm.push_op(Num.new(ri))
    end
  }
  define_op("cleartomark"){|vm| vm.pop_to_mark }

  # dictionary operators
  define_op("def"){|vm|
    v = vm.pop_op
    k = vm.pop_op(:literal_name).to_s
    vm.current_dict[k] = v
  }
  define_op("dict"){|vm|
    n = vm.pop_op.ensure_value_type(Integer)
    vm.push_op(Value.new(:dict, Hash.new))
  }
  define_op("begin"){|vm|
    d = vm.pop_op(:dict)
    vm.push_dict(d)
  }
  define_op("end"){|vm|
    vm.pop_dict
  }
  define_op("load"){|vm|
    k = vm.pop_op(:literal_name).to_s
    v = vm.lookup_dict(k)
    if v
      vm.push_op(v)
    end
  }

  # boolean operators
  define_op("eq"){|vm|
    x = vm.pop_op
    y = vm.pop_op
    vm.push_op(x.eq(y) ? VTRUE : VFALSE)
  }
  define_op("ne"){|vm|
    x = vm.pop_op
    y = vm.pop_op
    vm.push_op(x.eq(y) ? VFALSE : VTRUE)
  }
  define_op("gt"){|vm|
    y = vm.pop_op
    x = vm.pop_op
    vm.push_op(x.cmp(y) > 0 ? VTRUE : VFALSE)
  }
  define_op("lt"){|vm|
    y = vm.pop_op
    x = vm.pop_op
    vm.push_op(x.cmp(y) < 0 ? VTRUE : VFALSE)
  }
  define_op("ge"){|vm|
    y = vm.pop_op
    x = vm.pop_op
    vm.push_op(x.cmp(y) >= 0 ? VTRUE : VFALSE)
  }
  define_op("le"){|vm|
    y = vm.pop_op
    x = vm.pop_op
    vm.push_op(x.cmp(y) <= 0 ? VTRUE : VFALSE)
  }
  define_op("and"){|vm|
    x = vm.pop_op(:bool).value
    y = vm.pop_op(:bool).value
    vm.push_op(x && y ? VTRUE : VFALSE)
  }
  define_op("or"){|vm|
    x = vm.pop_op(:bool).value
    y = vm.pop_op(:bool).value
    vm.push_op(x || y ? VTRUE : VFALSE)
  }
  define_op("xor"){|vm|
    x = vm.pop_op(:bool).value
    y = vm.pop_op(:bool).value
    vm.push_op(x == y ? VFALSE : VTRUE)
  }
  define_op("not"){|vm|
    b = vm.pop_op(:bool)
    vm.push_op(b.value ? VFALSE : VTRUE)
  }

  # higher-order operators
  define_op("exec"){|vm|
    proc = vm.pop_op(:procedure)
    proc[vm]
  }
  define_op("ifelse"){|vm|
    p2 = vm.pop_op(:procedure)
    p1 = vm.pop_op(:procedure)
    b = vm.pop_op(:bool)
    (b.value ? p1 : p2)[vm]
  }
  define_op("if"){|vm|
    p1 = vm.pop_op(:procedure)
    b = vm.pop_op(:bool)
    if b.value
      p1[vm]
    end
  }
  define_op("repeat"){|vm|
    proc = vm.pop_op(:procedure, :operator)
    n = vm.pop_op.ensure_value_type(Integer).value
    catch (:exit) {
      n.times{|_| proc[vm] }
    }
  }
  define_op("exit"){|vm|
    throw :exit
  }
  define_op("loop"){|vm|
    proc = vm.pop_op(:procedure)
    catch (:exit) {
      while true
        proc[vm]
      end
    }
  }
  define_op("for"){|vm|
    proc = vm.pop_op(:procedure)
    limit = vm.pop_op(:numeric).value
    inc = vm.pop_op(:numeric).value
    i = vm.pop_op(:numeric).value
    catch (:exit) {
      until (inc > 0 && i > limit) || (inc < 0 && i < limit)
        vm.push_op(Num.new(i))
        proc[vm]
        i += inc
      end
    }
  }
  define_op("forall"){|vm|
    proc = vm.pop_op(:procedure)
    v = vm.pop_op(:string, :array)
    a = v.is_a?(Str) ? v.to_s.bytes : v.value
    catch (:exit) {
      a.each{|e|
        vm.push_op(e)
        proc[vm]
      }
    }
  }

  # array operators
  define_op("array"){|vm|
    l = vm.pop_op().ensure_value_type(Integer).value
    a = Value.new(:array, [VNULL] * l)
    vm.push_op(a)
  }
  define_op("astore"){|vm|
    a = vm.pop_op(:array)
    l = a.value.length
    l.times{|i| a.value[l - i - 1] = vm.pop_op }
    vm.push_op(a)
  }
  define_op("aload"){|vm|
    a = vm.pop_op(:array)
    a.value.each{|v|
      vm.push_op(v)
    }
    vm.push_op(a)
  }

  # string operators
  define_op("string"){|vm|
    v = vm.pop_op().ensure_value_type(Integer)
    vm.push_op(Str.new("\0" * v.value))
  }
  define_op("length"){|vm|
    l = vm.pop_op(:string).len
    vm.push_op(Num.new(l))
  }
  define_op("get"){|vm|
    i = vm.pop_op().ensure_value_type(Integer).value
    v = vm.pop_op(:string, :array)
    if v.is_a?(Str)
      if i >= v.len then
        raise "range error"
      end
      vm.push_op(Num.new(v.to_s[i].ord))
    else
      if i > v.value.size
        raise "range error"
      end
      vm.push_op(v.value[i])
    end
  }
  define_op("put"){|vm|
    e = vm.pop_op
    i = vm.pop_op().ensure_value_type(Integer).value
    v = vm.pop_op(:string, :array)
    if v.is_a?(Str)
      if i >= v.len then
        raise "range error"
      end
      v.write(i, e.ensure_value_type(Integer).value.chr)
    else
      if i > v.value.size
        raise "range error"
      end
      v.value[i] = e
    end
  }
  define_op("getinterval"){|vm|
    # string index count 'getinterval' substring
    count = vm.pop_op().ensure_value_type(Integer).value
    index = vm.pop_op().ensure_value_type(Integer).value
    str = vm.pop_op(:string)
    vm.push_op(str.slice(index, count))
  }
  define_op("putinterval"){|vm|
    # string1 index string2 putinterval −
    str1 = vm.pop_op(:string)
    index = vm.pop_op().ensure_value_type(Integer).value
    str2 = vm.pop_op(:string)
    str1.write(index, str2.to_s)
    str1[index, str2.length] = str2
  }
  define_op("cvi"){|vm|
    v = vm.pop_op(:string, :numeric)
    case v.type
    when :string
      sc = Scanner.new(v.value)
      tk = Parser.read_num_or_name(sc)
      if tk[:type] != :numeric
        raise TypeError, "cannot convert #{v.to_readable} to int"
      else
        i = Num.new(tk[:val].to_i)
      end
    when :numeric
      i = Num.new(v.value.to_i)
    end
    vm.push_op(i)
  }
  define_op("cvr"){|vm|
    v = vm.pop_op(:string, :numeric)
    case v.type
    when :string
      sc = Scanner.new(v.value)
      tk = Parser.read_num_or_name(sc)
      if tk[:type] != :numeric
        raise "type error: cannot convert #{v.to_readable} to real"
      else
        r = Num.new(tk[:val].to_f)
      end
    when :numric
      r = Num.new(v.value.to_f)
    end
    vm.push_op(r)
  }
  define_op("cvn"){|vm|
    s = vm.pop_op(:string)
    vm.push_op(Value.new(:literal_name, s.value))
  }
  define_op("cvs"){|vm|
    buf = vm.pop_op(:string)
    s = vm.pop_op.to_s
    l = buf.write(0, s)
    vm.push_op(buf.slice(0, l))
  }
  define_op("cvrs"){|vm|
    buf = vm.pop_op(:string)
    b = vm.pop_op.ensure_value_type(Integer).value
    i = vm.pop_op(:numeric).value.to_i
    s = i.to_s(b)
    l = buf.write(0, s)
    vm.push_op(buf.slice(0, l))
  }

  # numeric operators
  define_op("add"){|vm|
    a = vm.pop_op(:numeric)
    b = vm.pop_op(:numeric)
    vm.push_op(Num.new(b.value + a.value))
  }
  define_op("sub"){|vm|
    a = vm.pop_op(:numeric)
    b = vm.pop_op(:numeric)
    vm.push_op(Num.new(b.value - a.value))
  }
  define_op("mul"){|vm|
    a = vm.pop_op(:numeric)
    b = vm.pop_op(:numeric)
    vm.push_op(Num.new(b.value * a.value))
  }
  define_op("mod"){|vm|
    a = vm.pop_op(:numeric).ensure_value_type(Integer)
    b = vm.pop_op(:numeric).ensure_value_type(Integer)
    vm.push_op(Num.new(b.value % a.value))
  }
  define_op("div"){|vm|
    a = vm.pop_op(:numeric)
    b = vm.pop_op(:numeric)
    vm.push_op(Num.new(b.value.to_f / a.value))
  }
  define_op("idiv"){|vm|
    a = vm.pop_op(:numeric).ensure_value_type(Integer)
    b = vm.pop_op(:numeric).ensure_value_type(Integer)
    vm.push_op(Num.new(b.value / a.value))
  }
  define_op("abs"){|vm|
    a = vm.pop_op(:numeric)
    vm.push_op(Num.new(a.value.abs))
  }
  define_op("neg"){|vm|
    a = vm.pop_op(:numeric)
    vm.push_op(Num.new(-a.value))
  }
  define_op("ceiling"){|vm|
    a = vm.pop_op(:numeric)
    vm.push_op(Num.new(a.value.ceil))
  }
  define_op("floor"){|vm|
    a = vm.pop_op(:numeric)
    vm.push_op(Num.new(a.value.floor))
  }
  define_op("round"){|vm|
    a = vm.pop_op(:numeric)
    vm.push_op(Num.new(a.value.round))
  }
  define_op("truncate"){|vm|
    a = vm.pop_op(:numeric)
    vm.push_op(Num.new(a.value.truncate))
  }
  define_op("sqrt"){|vm|
    a = vm.pop_op(:numeric)
    vm.push_op(Num.new(Math.sqrt(a.value)))
  }
  define_op("sin"){|vm|
    a = vm.pop_op(:numeric)
    vm.push_op(Num.new(Math.sin(deg2rad(a.value))))
  }
  define_op("cos"){|vm|
    a = vm.pop_op(:numeric)
    vm.push_op(Num.new(Math.cos(deg2rad(a.value))))
  }
  define_op("tan"){|vm|
    a = vm.pop_op(:numeric)
    vm.push_op(Num.new(Math.tan(deg2rad(a.value))))
  }
  define_op("atan"){|vm|
    d = vm.pop_op(:numeric)
    n = vm.pop_op(:numeric)
    vm.push_op(Num.new(rad2deg(Math.atan2(n.value, d.value)) % 360))
  }
  define_op("exp"){|vm|
    e = vm.pop_op(:numeric)
    b = vm.pop_op(:numeric)
    vm.push_op(Num.new(b.value ** e.value))
  }
  define_op("ln"){|vm|
    a = vm.pop_op(:numeric)
    vm.push_op(Num.new(Math.log(a.value)))
  }
  define_op("log"){|vm|
    a = vm.pop_op(:numeric)
    vm.push_op(Num.new(Math.log10(a.value)))
  }
  define_op("rand"){|vm|
    vm.push_op(Num.new(vm.rand))
  }
  define_op("srand"){|vm|
    seed = vm.pop_op(:numeric).ensure_value_type(Integer)
    vm.rand_set_seed(seed.value)
  }
  define_op("rrand"){|vm|
    vm.push_op(Num.new(vm.rand_get_seed))
  }

  # graphics operators
  define_op("showpage"){|vm| vm.break_page }
  define_op("currentpoint"){|vm|
    raise "no current point" if !vm.drawing?
    v = vm.point
    vm.push_op(Num.new(v[0]))
    vm.push_op(Num.new(v[1]))
  }
  define_op("translate"){|vm|
    dy = vm.pop_op(:numeric).value
    dx = vm.pop_op(:numeric).value
    vm.apply_matrix(Matrix[[1, 0, dx], [0, 1, dy], [0, 0, 1]])
    if vm.drawing?
      vm.move_point(Vector[-dx, -dy])
    end
  }
  define_op("rotate"){|vm|
    t = deg2rad(vm.pop_op(:numeric).value)
    rot = Matrix[
      [Math.cos(t), -Math.sin(t), 0],
      [Math.sin(t), Math.cos(t), 0],
      [0, 0, 1]
    ]
    vm.apply_matrix(rot)
    if vm.drawing?
      cp = vm.point
      np = rot.inverse * Vector[cp[0], cp[1], 1]
      vm.set_point(Vector[np[0], np[1]])
    end
  }
  define_op("scale"){|vm|
    sy = vm.pop_op(:numeric).value
    sx = vm.pop_op(:numeric).value
    vm.apply_matrix(Matrix[[sx, 0, 0], [0, sy, 0], [0, 0, 1]])
    v = vm.point
    if vm.drawing?
      vm.set_point(Vector[v[0] / sx, v[1] / sy])
    end
  }
  define_op("setrgbcolor"){|vm|
    b = vm.pop_op(:numeric).value
    g = vm.pop_op(:numeric).value
    r = vm.pop_op(:numeric).value
    vm.set_color([r, g, b])
  }
  define_op("setcmykcolor"){|vm| raise "not implemented"}
  define_op("setfont"){|vm|
    fd = vm.pop_op(:dict)
    if !(fd.value.has_key?(:name) && fd.value.has_key?(:size))
      raise "invalid font"
    end
    vm.set_font(fd)
  }
  
  define_op("findfont"){|vm|
    name = vm.pop_op(:literal_name, :string).to_s
    vm.push_op(Value.new(:dict, {name: name, size: 1.0}))
  }
  define_op("scalefont"){|vm|
    sc = vm.pop_op(:numeric).value
    fd = vm.pop_op(:dict).value.dup
    fd[:size] *= sc
    vm.push_op(Value.new(:dict, fd))
  }
  define_op("show"){|vm|
    raise "no current point" if !vm.drawing?
    SYSTEM_DICT.value["dup"][vm]
    SYSTEM_DICT.value["stringwidth"][vm]
    h = vm.pop_op(:numeric).value
    w = vm.pop_op(:numeric).value
    str = vm.pop_op(:string).to_s
    v = vm.real_point
    font = vm.font.value
    vm.add_painted(Text.new(vm.color, font[:name], font[:size], v[0], v[1], str))
    vm.move_point(Vector[w, 0])
    vm.add_path(:moveto, vm.real_point)
  }
  
  define_op("ashow"){|vm| raise "not implemented"}
  define_op("widthshow"){|vm| raise "not implemented"}
  define_op("stringwidth"){|vm|
    l = vm.pop_op(:string).len
    vm.push_op(Num.new(l * vm.font.value[:size] * 0.6))
    vm.push_op(Num.new(0.0))
  }
  define_op("charpath"){|vm| raise "not implemented"}
  define_op("setlinewidth"){|vm|
    vm.gs.linewidth = vm.pop_op(:numeric).value
  }
  define_op("newpath"){|vm|
    vm.init_path
  }
  define_op("moveto"){|vm|
    y = vm.pop_op(:numeric).value
    x = vm.pop_op(:numeric).value
    vm.set_point(Vector[x, y])
    vm.add_path(:moveto, vm.real_point)
  }
  define_op("rmoveto"){|vm|
    y = vm.pop_op(:numeric).value
    x = vm.pop_op(:numeric).value
    vm.move_point(Vector[x, y])
    vm.add_path(:moveto, vm.real_point)
  }
  define_op("lineto"){|vm|
    y = vm.pop_op(:numeric).value
    x = vm.pop_op(:numeric).value
    vm.set_point(Vector[x, y])
    vm.add_path(:lineto, vm.real_point)
  }
  define_op("rlineto"){|vm|
    y = vm.pop_op(:numeric).value
    x = vm.pop_op(:numeric).value
    vm.move_point(Vector[x, y])
    vm.add_path(:lineto, vm.real_point)
  }

  def arcbody(vm, tag)
    ang2 = vm.pop_op(:numeric).value
    ang1 = vm.pop_op(:numeric).value
    r = vm.pop_op(:numeric).value
    oy = vm.pop_op(:numeric).value
    ox = vm.pop_op(:numeric).value

    s = vm.transform_point(Vector[ox + r * Math.cos(deg2rad(ang1)), oy + r * Math.sin(deg2rad(ang1))])
    e = vm.transform_point(Vector[ox + r * Math.cos(deg2rad(ang2)), oy + r * Math.sin(deg2rad(ang2))])
    if !vm.drawing?
      vm.set_point(s)
      vm.add_path(:moveto, vm.real_point)
    elsif vm.real_point != s
      vm.add_path(:lineto, s)
    end
    vm.set_point(e)
    vm.add_path(tag, vm.transform_point(Vector[ox, oy]), r, ang1, ang2)
  end
  private_module_function :arcbody

  define_op("arc"){|vm| arcbody(vm, :arc) }
  define_op("arcn"){|vm| arcbody(vm, :arcn) }
  define_op("arct"){|vm| raise "not implemented"}
  define_op("arcto"){|vm| raise "not implemented"}
  define_op("curveto"){|vm|
    y3 = vm.pop_op(:numeric).value
    x3 = vm.pop_op(:numeric).value
    y2 = vm.pop_op(:numeric).value
    x2 = vm.pop_op(:numeric).value
    y1 = vm.pop_op(:numeric).value
    x1 = vm.pop_op(:numeric).value

    v1 = vm.transform_point(Vector[x1, y1])
    v2 = vm.transform_point(Vector[x2, y2])
    v3 = vm.transform_point(Vector[x3, y3])
    vm.set_point(v3)
    vm.add_path(:curveto, v1, v2, v3)
  }
  define_op("rcurveto"){|vm|
    y3 = vm.pop_op(:numeric).value
    x3 = vm.pop_op(:numeric).value
    y2 = vm.pop_op(:numeric).value
    x2 = vm.pop_op(:numeric).value
    y1 = vm.pop_op(:numeric).value
    x1 = vm.pop_op(:numeric).value

    c = vm.point
    v1 = vm.transform_point(c + Vector[x1, y1])
    v2 = vm.transform_point(c + Vector[x2, y2])
    v3 = vm.transform_point(c + Vector[x3, y3])
    vm.set_point(v3)
    vm.add_path(:curveto, v1, v2, v3)
  }
  define_op("closepath"){|vm|
    return if !vm.drawing? || vm.current_path.size < 2
    vm.set_point(vm.subpath_starting_point)
    vm.add_path(:closepath)
  }
  define_op("fill"){|vm|
    vm.add_painted(PathF.new(vm.color, vm.current_path, :nonzero))
    vm.init_path
  }
  define_op("eofill"){|vm| raise "not implemented"
    vm.add_painted(PathF.new(vm.color, vm.current_path, :evenodd))
    vm.init_path
  }
  define_op("stroke"){|vm|
    vm.add_painted(PathS.new(vm.color, vm.linewidth, vm.current_path))
    vm.init_path
  }

  define_op("clip"){|vm| raise "not implemented"}
  define_op("eoclip"){|vm| raise "not implemented"}
  define_op("initclip"){|vm| raise "not implemented"}

  define_op("gsave"){|vm| vm.push_gs }
  define_op("grestore"){|vm| vm.pop_gs }

  # system operators
  define_op("quit"){|vm| exit }
  define_op("run"){|vm|
    filename = vm.pop_op(:string).to_s
    vm.eval_prog(Parser.readfile(filename))
  }
  define_op("version"){|vm| vm.push_op(Str.new(VERSION)) }
  define_op("product"){|vm| vm.push_op(Str.new(PRODUCT_NAME)) }

  # extensions
  define_op(".o"){|vm|
    vm.ostack.reverse.each_with_index{|v,i| puts "#{i}: #{v.to_readable}" }
  }
  define_op(".g"){|vm|
    puts "[pages]"
    p vm.pages
    puts "[current page]"
    p vm.current_page
    puts "[current graphics state]"
    p vm.gs
    puts "[current graphics stack]"
    p vm.gstack
  }
  define_op(".writesvg"){|vm|
    h = vm.pop_op(:numeric).value
    w = vm.pop_op(:numeric).value
    y = vm.pop_op(:numeric).value
    x = vm.pop_op(:numeric).value
    basename = vm.pop_op(:string).to_s
    if vm.current_page.size > 1
      vm.break_page
    end
    SVG.new(vm.pages, [x, y, w, h]).write_files(basename)
    vm.pages.clear
    vm.init_path
  }
  
  class Paint
    attr_reader :color
    def initialize(color)
      @color = color.dup
    end
  end
  
  class PathF < Paint
    attr_reader :path, :fillrule
    def initialize(color, path, fillrule)
      super color
      @path = path
      @fillrule = fillrule
    end
  end
  
  class PathS < Paint
    attr_reader :path, :linewidth
    def initialize(color, linewidth, path)
      super color
      @linewidth = linewidth
      @path = path
    end
  end
  
  class Text < Paint
    attr_reader :fontname, :fontsize, :x, :y, :text
    def initialize(color, fontname, fontsize, x, y, text)
      super color
      @fontname = fontname
      @fontsize = fontsize
      @x = x
      @y = y
      @text = text
    end
  end


  class GraphicsState
    attr_accessor :color, :linewidth, :font, :current_path, :point, :ctm
    
    def initialize
      @color = [0, 0, 0]
      @linewidth = 1
      @font = nil
      @point = nil
      @ctm = Matrix[[1, 0, 0], [0, 1, 0], [0, 0, 1]]
      @current_path = []
    end

    def dup
      super.tap{|d|
        d.color = d.color.dup
        d.point = d.point.dup
        d.ctm = d.ctm.dup
        d.current_path = d.current_path.dup
      }
    end
  end

  class VM
    attr_reader :ostack, :dstack, :gstack, :gs, :current_page, :pages, :dsc
    attr_accessor :stdout, :stderr, :step

    def initialize
      @ostack = []  # operand stack
      @dstack = [SYSTEM_DICT, Value.new(:dict, {})]  # dict stack
      
      @gstack = []  # graphics state stack
      @gs = GraphicsState.new # current graphics state
      @current_page = []  # painted elements
      @pages = []  # rendered pages
      @dsc = Hash.new
      
      @step = false
      @stdout = $stdout
      @stderr = $stderr
      @saved = nil
      @nametable = Hash.new
      @rand = Random.new
    end

    def save_states
      @saved = [@ostack.dup, @gstack.dup, @dstack.dup, @gs.dup, @current_page.dup, @pages.dup, @rand.dup]
    end

    def restore_states
      if @saved.nil?
        raise "states not saved"
      end
      @ostack, @gstack, @dstack, @gs, @current_page, @pages, @rand = @saved
    end

    def rand
      @rand.rand(0xffffffff)
    end

    def rand_set_seed(seed)
      @rand = Random.new(seed)
    end

    def rand_get_seed
      @rand.seed
    end

    def push_op(o)
      @ostack << o
    end

    def pop_op(*typecheck)
      if @ostack.empty?
        raise "stack underflow"
      end
      v = @ostack.pop
      if !typecheck.empty? && !typecheck.include?(v.type)
        raise "type error: #{v.to_readable} is not a " + typecheck * " nor a "
      end
      v
    end

    def pop_to_mark
      if @ostack.index{|v| v.type == :mark}.nil?
        raise "unmatched mark"
      end
      buf = []
      while true
        v = pop_op
        if v.eq(VMARK)
          break
        else
          buf.unshift(v)
        end
      end
      buf
    end

    def push_dict(d)
      @dstack << d
    end

    def pop_dict()
      if @dstack.size == 2
        raise "dict stack underflow"
      end
      @dstack.pop
    end

    def current_dict
      @dstack[-1].value
    end

    def lookup_dict(k)
      @dstack.reverse_each{|d|
        if d.value.has_key?(k)
          return d.value[k]
        end
      }
      nil
    end

    def drawing?
      !@gs.point.nil?
    end

    def init_path
      @gs.point = @gs.current_path = nil
    end

    def current_path
      @gs.current_path.dup
    end

    def color
      @gs.color.dup
    end

    def set_color(c)
      @gs.color = c
    end

    def font
      @gs.font.dup
    end

    def set_font(fd)
      @gs.font = fd
    end

    def point
      @gs.point.dup
    end

    def set_point(v)
      if !drawing?
        @gs.current_path = []
      end
      @gs.point = v
    end

    def move_point(v)
      raise "no current point" if !drawing?
      @gs.point = @gs.point + v
    end

    def init_matrix
      @gs.ctm = Matrix[[1, 0, 0], [0, 1, 0], [0, 0, 1]]
    end

    def apply_matrix(m)
      @gs.ctm = @gs.ctm * m
    end

    def transform_point(v)
      t = @gs.ctm * Vector[v[0], v[1], 1]
      Vector[t[0], t[1]]
    end

    def real_point
      transform_point @gs.point
    end

    def add_path(com, *args)
      raise "no current point" if !drawing?
      @gs.current_path << [com, *args]
    end

    def subpath_starting_point
      raise "no current point" if !drawing?
      mv = @gs.current_path.reverse.find{|sp| sp[0] == :moveto || sp[0] == :rmoveto }
      raise "invalid graphics state" if mv.nil?
      mv[1]
    end

    def add_painted(g)
      raise "#{g.inspect} is not a Paint object" if !g.is_a?(Paint)
      @current_page << g
    end

    def break_page
      @pages << @current_page
      @current_page = []
    end

    def push_gs
      @gstack << @gs
      @gs = @gs.dup
    end

    def pop_gs
      if @gstack.empty?
        @gs = GraphicsState.new
      else
        @gs = @gstack.pop
      end
    end

    def eval1(expr)
      begin
        case expr[:type]
        when :executable_name
          name = expr[:val]
          v = lookup_dict(name)
          if v.nil?
            raise "undefined name: #{name}"
          end

          case v.type
          when :operator, :procedure
            v[self]
          else
            push_op(v)
          end
        when :numeric
          push_op(Num.new(expr[:val]))
        when :string
          push_op(Str.new(expr[:val]))
        when :literal_name
          v = @nametable[expr[:val]] || Value.new(:literal_name, expr[:val])
          push_op(v)
        when :executable_array
          push_op(Procedure.new(expr[:val]))
        else
          raise "unknown token: #{expr.inspect}"
        end
      rescue
        $stderr.puts "error in #{expr.inspect}"
        raise
      end
    end

    def eval_prog(prog)
      prog.each{|expr|
        if @step
          $stderr.puts "[current stack] : " + @ostack.map(&:to_readable) * ", "
          $stderr.puts "[next inst] : " + Parser.expr_to_s(expr)
          $stderr.print "press enter to continue ('q' to quit)"
          if $stdin.readline[0] == "q"
            exit
          end
        end
        case expr[:type]
        when :comment
          if expr[:val] =~ /^%!(PS.+)$/
            @dsc[:dscver] = $1
          elsif expr[:val] =~ /^%%([^:]+):(.+)$/
            @dsc[$1] = $2.strip
          end
        else
          eval1(expr)
        end
      }
    end

    def eval_string(src)
      eval_prog(Parser.parse_string(src))
    end
  end

  def self.run_file(f, opts)
    vm = VM.new
    vm.step = opts[:step]
    vm.eval_prog(Parser.readfile(f))
    if !vm.current_page.empty?
      vm.break_page
    end
    if !vm.pages.empty?
      basename = File.join(File.dirname(f), File.basename(f, ".*"))
      bbox = (vm.dsc["BoundingBox"] || DEFAULT_BBOX).split(/\s+/).map(&:to_i)
      SVG.new(vm.pages, bbox).write_files(basename)
    end
  end

  def self.start_repl(opts)
    vm = VM.new
    if opts[:step]
      vm.step = true
    end

    cont = false
    buf = ""
    while true
      if !cont
        print "minips"
      else
        print "   ..."
      end
      print vm.ostack.empty? ? ">" : "[#{vm.ostack.size}]>"
      l = $stdin.gets
      buf << l
      begin
        vm.eval_string(buf)
        buf = ""
        cont = false
      rescue Parser::EofError => e
        cont = true
      rescue => e
        $stderr.puts e.inspect
        buf = ""
        cont = false
      end
    end
  end


  class SVG
    include Util
    
    def initialize(pages, bbox)
      @pages = pages
      @x, @y, @w, @h = bbox
    end

    def arccmd(o, r, ang1, ang2, d)
      rad1 = deg2rad(ang1 % 360)
      rad2 = deg2rad(ang2 % 360)
      ox = o[0]
      oy = o[1]
      ex = ox + r * Math.cos(rad2)
      ey = @h - oy + r * Math.sin(rad2)
      if ang2 - ang1 >= 360
        mx = ox + r * Math.cos(rad1 + Math::PI)
        my = @h - oy + r * Math.sin(rad1 + Math::PI)
        "A#{r} #{r} 0 0 0 #{mx} #{my}A#{r} #{r} 0 0 0 #{ex} #{ey}"
      else
        la = (ang2 - ang1) % 360 > 180
        "A" + [r, r, 0, la ? d : (1 - d), d, ex, ey] * " "
      end
    end

    def p_to_s(v)
      "#{v[0]} #{@h-v[1]}"
    end

    def svgpath(path)
      buf = ""
      path.each{|cmd,*args|
        buf << case cmd
        when :moveto    then "M" + p_to_s(args[0])
        when :lineto    then "L" + p_to_s(args[0])
        when :curveto   then "C" + args.map{|v| p_to_s(v) } * " "
        when :arc       then arccmd(*args, 0)
        when :arcn      then arccmd(*args, 1)
        when :closepath then "z"
        else
          raise "unsupported command :#{cmd}"
        end
      }
      buf
    end

    public
    def write_files(basename)
      filename = basename + (@pages.count == 1 ? ".svg" : "_%d.svg")
      @pages.each_with_index{|page, i|
        puts "writing #{filename} ..."
        viewbox = [@x, @y, @w, @h] * " "
        File.open(filename, "w"){|fo|
          fo.puts('<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="%d" height="%d" viewBox="%s">' % [@w, @h, viewbox])
          page.each {|e|
            case e
            when PathF
              fo.puts('<path fill="#%s" fill-rule="%s" d="%s"/>' % [color_to_hex(e.color), e.fillrule, svgpath(e.path)])
            when PathS
              fo.puts('<path fill="none" stroke="#%s" stroke-width="%s" d="%s"/>' % [color_to_hex(e.color), e.linewidth, svgpath(e.path)])
            when Text
              fontname = FONTMAP[e.fontname] || e.fontname
              fo.puts('<text x="%s" y="%s" fill="#%s" font-family="%s,sans-serif" font-size="%s">%s</text>' % [e.x, @h - e.y, color_to_hex(e.color), "'#{fontname}'", e.fontsize, e.text])
            end
          }
          fo.puts "</svg>"
        }
      }
      puts "done"
    end
  end
end

if $0 == __FILE__
  opts = {interactive: false, step: false}
  while $*[0] && $*[0] =~ /^-/
    sw = $*.shift
    case sw
    when "-i"
      opts[:interactive] = true
    when "-s"
      opts[:step] = true
    else
      $stderr.puts "[warn] unknown switch #{-sw}"
    end
  end
  if $*.size == 0 && !opts[:interactive]
    puts "usage: ruby minps.rb src.ps"
  end

  if opts[:interactive]
    MiniPS.start_repl(opts)
  else
    MiniPS.run_file($*[0], opts)
  end
end
