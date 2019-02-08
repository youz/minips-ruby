require "test/unit"
require_relative "minips"

class TestParser < Test::Unit::TestCase
  def setup
  end
  
  def test_parse_int
    src = "123 456"
    
    tk1, tk2 = MiniPS::Parser.parse_string(src)

    assert_equal(:numeric, tk1[:type])
    assert_equal(123, tk1[:val])
    assert_equal(:numeric, tk2[:type])
    assert_equal(456, tk2[:val])
  end

  def test_parse_float
    src = "123.45 2.3e-4"

    tk1, tk2 = MiniPS::Parser.parse_string(src)

    assert_equal(:numeric, tk1[:type])
    assert_equal(123.45, tk1[:val])
    assert_equal(:numeric, tk2[:type])
    assert_equal(2.3e-4, tk2[:val])
  end

  def test_parse_nbased_int
    src = "2#1111 16#ffff 36#zz"

    tk1, tk2, tk3 = MiniPS::Parser.parse_string(src)

    assert_equal(:numeric, tk1[:type])
    assert_equal(2 ** 4 - 1, tk1[:val])
    assert_equal(:numeric, tk2[:type])
    assert_equal(16 ** 4 - 1, tk2[:val])
    assert_equal(:numeric, tk3[:type])
    assert_equal(36 ** 2 - 1, tk3[:val])
  end

  def test_parse_string
    src = <<__STRING_TEST__
(asdf)
(qw\
er)
__STRING_TEST__

    tk1, tk2 = MiniPS::Parser.parse_string(src)

    assert_equal(:string, tk1[:type])
    assert_equal("asdf", tk1[:val])
    assert_equal(:string, tk2[:type])
    assert_equal("qwer", tk2[:val])
  end

  def test_parse_executable_name
    src = "asdf qwer"

    tk1, tk2 = MiniPS::Parser.parse_string(src)

    assert_equal(:executable_name, tk1[:type])
    assert_equal("asdf", tk1[:val])
    assert_equal(:executable_name, tk2[:type])
    assert_equal("qwer", tk2[:val])
  end
  
  def test_parse_literal_name
    src = "/asdf /qwer"

    tk1, tk2 = MiniPS::Parser.parse_string(src)

    assert_equal(:literal_name, tk1[:type])
    assert_equal("asdf", tk1[:val])
    assert_equal(:literal_name, tk2[:type])
    assert_equal("qwer", tk2[:val])
  end

  def test_parse_executable_array
    src = "{dup mul 2 mul}"

    a = MiniPS::Parser.parse_string(src)[0]

    assert_equal(:executable_array, a[:type])
    assert_equal(["dup", "mul", 2, "mul"], a[:val].map{|v| v[:val]})
  end

  def test_parse_array
    src = "[0 1 2]"

    parsed = MiniPS::Parser.parse_string(src)

    assert_equal(5, parsed.size)
    assert_equal(:executable_name, parsed[0][:type])
    assert_equal("[", parsed[0][:val])
    assert_equal(:executable_name, parsed[-1][:type])
    assert_equal("]", parsed[-1][:val])
  end

  def test_parse_comment
    src = <<__COMMENT_TEST__
123 % test
456
__COMMENT_TEST__

    tk1, c, tk2 = MiniPS::Parser.parse_string(src)

    assert_equal(:numeric, tk1[:type])
    assert_equal(123, tk1[:val])
    assert_equal(:comment, c[:type])
    assert_equal("% test", c[:val])
    assert_equal(:numeric, tk2[:type])
    assert_equal(456, tk2[:val])
  end

end


class TestStr < Test::Unit::TestCase

  def test_str_slice
    buf = MiniPS::Str.new(" " * 20)
    s1 = buf.slice(0, 10)
    s2 = s1.slice(3, 5)
    buf.value[0, 5] = "aaaaa"

    assert_equal("aaaaa               ", buf.to_s)
    assert_equal("aaaaa     ", s1.to_s)
    assert_equal("aa   ", s2.to_s)
  end

  def test_str_to_readable
    s1 = MiniPS::Str.new("\n\r\t")
    s2 = s1.slice(1, 2)

    assert_equal("(\\n\\r\\t)", s1.to_readable)
    assert_equal("(\\r\\t)", s2.to_readable)
  end
end

class TestValue < Test::Unit::TestCase
  def setup
    @vm = MiniPS::VM.new
  end
  
  def test_value_numeric_to_s
    @vm.eval_string("1.23")
    v = @vm.pop_op
    assert_equal("1.23", v.to_s)
  end

  def test_value_string_to_s
    @vm.eval_string("(as\\(df\\))")
    v = @vm.pop_op
    assert_equal("as(df)", v.to_s)
  end

  def test_value_literal_to_s
    @vm.eval_string("/asdf")
    v = @vm.pop_op
    assert_equal("asdf", v.to_s)
  end

  def test_value_string_to_readable
    @vm.eval_string("(as\\(df\\))")
    v = @vm.pop_op
    assert_equal("(as\\(df\\))", v.to_readable)

    @vm.eval_string("((\\b\\f\\n\\r\\t))")
    v = @vm.pop_op
    assert_equal("(\\(\\b\\f\\n\\r\\t\\))", v.to_readable)
  end
  
  def test_value_literal_to_readable
    @vm.eval_string("/asdf")
    v = @vm.pop_op
    assert_equal("/asdf", v.to_readable)
  end
end

class TestVM < Test::Unit::TestCase
  def setup
    @vm = MiniPS::VM.new
  end

  def teardown
    @vm = nil
  end

  class TestValues < TestVM
    setup
    def test_vm_eval_numeric
      src = "123 4.5 16#ffff"

      @vm.eval_string(src)

      v1 = @vm.pop_op
      v2 = @vm.pop_op
      v3 = @vm.pop_op

      assert_equal(123, v3.value)
      assert_equal(4.5, v2.value)
      assert_equal(65535, v1.value)
    end

    def test_vm_eval_array
      src = "[123 (asdf) /qwer]"

      @vm.eval_string(src)
      a = @vm.pop_op

      assert_equal(:array, a.type)
      assert_equal(123, a.value[0].value)
      assert_equal("(asdf)", a.value[1].to_readable)
      assert_equal("/qwer", a.value[2].to_readable)
    end
  end

  class TestNumOperators < TestVM
    setup
    def test_binary_op
      @vm.eval_string("/x 12 def /y 5 def")
      [
        ["add", 17],
        ["sub", 7],
        ["mul", 60],
        ["div", 2.4],
        ["idiv", 2],
        ["mod", 2],
        ["lt", false],
        ["gt", true],
        ["eq", false]
      ].each{|op, expect|
        @vm.eval_string("x y #{op}")
        assert_equal(expect, @vm.pop_op.value)
      }
    end

    def test_unary_op
      [
        ["1 abs", 1],
        ["-1 abs", 1],
        ["5 neg", -5],
        ["-5 neg", 5],
        ["2.5 ceiling", 3.0],
        ["2.5 floor", 2.0],
        ["-1.8 floor", -2.0],
        ["2.2 round", 2.0],
        ["2.5 round", 3.0],
        ["2.7 round", 3.0],
        ["3.5 round", 4.0],
        ["2.5 truncate", 2.0],
        ["-2.5 truncate", -2.0]
      ].each{|code, expect|
        @vm.eval_string(code)
        assert_equal(expect, @vm.pop_op.value)
      }
    end

    def test_math_op
      [
        ["9 sqrt", 3.0],
        ["3 4 exp", 81.0],
        ["#{Math::E.to_s} ln", 1.0],
        ["1000 log", 3.0]
      ].each{|code, expect|
        @vm.eval_string(code)
        assert_equal(expect, @vm.pop_op.value)
      }
    end

    def test_trig_op
      %w(sin cos tan).each{|op|
        12.times{|i|
          t = MiniPS::Util.deg2rad(i * 30)
          @vm.eval_string("#{i * 30} #{op}")
          assert_equal(Math.send(op, t), @vm.pop_op.value)
        }
      }
      [
        ["0 1 atan", 0.0],
        ["1 0 atan", 90.0],
        ["0 -1 atan", 180.0],
        ["-1 0 atan", 270.0]
      ].each{|code, expect|
        @vm.eval_string(code)
        assert_equal(expect, @vm.pop_op.value)
      }
    end
  end

  class TestStrOperators < TestVM
    setup
    def test_strop_cvs
      @vm.eval_string("/buf 20 string def")
      nostr = "--nostringval--"
      [["123", "123"],
      ["4e-3", "0.004"],
      ["(abcd)", "abcd"],
      ["true", "true"],
      ["1 dict", nostr],
      ["[1 2 3]", nostr],
      ["{==}", nostr],
      ["[", nostr]].each{|src, expect|
        @vm.eval_string(src + " buf cvs")
        assert_equal(expect, @vm.pop_op(:string).to_s)
      }
    end
    
    def test_strop_cvs_sharing_buffer
      src = <<__CVS_TEST__
/b1 (123456789) def
/b2 /asdf b1 cvs def
/b3 42 b2 cvs def
b3 b2 b1
__CVS_TEST__
      
      @vm.eval_string(src)
      b1 = @vm.pop_op(:string)
      b2 = @vm.pop_op(:string)
      b3 = @vm.pop_op(:string)
      assert_equal("42df56789", b1.to_s)
      assert_equal("42df", b2.to_s)
      assert_equal("42", b3.to_s)
    end
  end

  class TestConditionalOperators < TestVM
    setup
    def test_if
      @vm.eval_string("0 1 lt {/ok} if")
      v = @vm.pop_op(:literal_name)
      assert_equal("/ok", v.to_readable)

      @vm.eval_string("1 0 lt {/ng} if count")
      v = @vm.pop_op(:numeric)
      assert_equal(0, v.value)
    end

    def test_ifelse
      @vm.eval_string("0 1 lt {/ok} {/ng} ifelse")
      v = @vm.pop_op(:literal_name)
      assert_equal("/ok", v.to_readable)

      @vm.eval_string("1 0 lt {/ok} {/ng} ifelse")
      v = @vm.pop_op(:literal_name)
      assert_equal("/ng", v.to_readable)
    end
  end
  
  class TestLoopOperators < TestVM
    setup
    def test_repeat
      @vm.eval_string("1 5 { dup add } repeat")
      v = @vm.pop_op(:numeric)
      assert_equal(32, v.value)
    end

    def test_for
      @vm.eval_string("0 1 1 10 { add } for")
      v = @vm.pop_op(:numeric)
      assert_equal(55, v.value)
    end

    def test_forall
      @vm.eval_string("1 [1 2 3 4 5] { mul } forall")
      v = @vm.pop_op(:numeric)
      assert_equal(120, v.value)
    end

    def test_loop
      @vm.eval_string("1 { dup add dup 1000 gt { exit } if } loop")
      v = @vm.pop_op(:numeric)
      assert_equal(1024, v.value)
    end
  end

  class TestProc < TestVM
    setup
    def test_exec
      @vm.eval_string("{42} exec")
      v = @vm.pop_op(:numeric)
      assert_equal(42, v.value)
    end

    def test_load
      @vm.eval_string("/m /mul load def 10 10 m")
      v = @vm.pop_op(:numeric)
      assert_equal(100, v.value)
    end

    def test_recursive_call
      src = "/fact { dup 1 eq { 1 } { dup 1 sub fact } ifelse mul } def 10 fact"
      @vm.eval_string(src)
      v = @vm.pop_op(:numeric)
      assert_equal(3628800, v.value)
    end
  end
end

