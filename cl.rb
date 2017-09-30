require 'sxp'
require 'ruby-beautify'

# CommonLisp の関数を実装するモジュール
module CL
  module_function 

  $random_state = Random.new(0)

  def print(v)
    Kernel.print "\n"
    Kernel.print v.inspect
    Kernel.print " "
    return v
  end

  def princ(v)
    return Kernel.print v
  end

  def read
    buf = ""
    while s = gets
      buf += s
      begin
        return SXP::Reader::CommonLisp.read(buf)
      rescue SXP::Reader::EOF
      end
    end
    return SXP::Reader::CommonLisp.read(buf)
  end

  def eval(sexp)
    return Kernel.eval(Translator.new.translate(sexp))
  end

  def terpri
    Kernel.print "\n"
    return nil
  end

  def get_internal_real_time
    Time.now.to_i * 1000
  end

  def let(*args)
    yield(*args)
  end

  def first(ls)
    ls[0]
  end

  def second(ls)
    ls[1]
  end

  def third(ls)
    ls[2]
  end

  def fourth(ls)
    ls[3]
  end

  def map(type, f, ary)
    ary.map(&f)
  end

  def truncate(num, divisor = 1)
    (num / divisor).to_i
  end

  def progn
    yield
  end

  def list(*args)
    args
  end

  def load(filename, *_)
    if filename =~ /\.lisp\z/
      fasl = filename.sub(/\.lisp\z/, ".rb")
      if !test("f", fasl) || test(">", filename, fasl)
        STDERR.puts "making fasl #{fasl}" if $DEBUG
        rb = ""
        SXP::Reader::CommonLisp.read_all(File.read(filename)).each do |sexp|
          rb.concat $translator.translate(sexp)
          rb.concat "\n"
        end
        rb.concat("\n") if rb[-1] != "\n"
        rb2 = RubyBeautify.pretty_string(rb, indent_token: ' ', indent_count: 2)
        open(fasl, "w") do |f|
          f.write(rb2)
        end
        Kernel.eval(rb2, $global_scope)
      else
        STDERR.puts "using fasl #{fasl}" if $DEBUG
        Kernel.eval(File.read(fasl), $global_scope)
      end
    else
      STDERR.puts "loading #{filename}" if $DEBUG
      rb = ""
      SXP::Reader::CommonLisp.read_all(File.read(filename)).each do |sexp|
        rb.concat $translator.translate(sexp)
        rb.concat "\n"
      end
      Kernel.eval(rb, $global_scope)
    end
    true
  end

  def cl_to_rb(sexp)
    rb = $translator.translate(sexp)
    rb.concat("\n") if rb[-1] != "\n"
    RubyBeautify.pretty_string(rb, indent_token: ' ', indent_count: 2)
  end

  def copy_tree(x)
    case x
    when Array
      x.dup
    else
      x
    end
  end

  def eq(a, b)
    a.object_id == b.object_id
  end

  def push_imp(elt, ls)
    if ls
      [elt] + ls
    else
      [elt]
    end
  end

  def translate_file(filename)
    rb = ""
    SXP::Reader::CommonLisp.read_all(File.read(filename)).each do |sexp|
      rb.concat $translator.translate(sexp)
      rb.concat "\n"
    end
    RubyBeautify.pretty_string(rb, indent_token: ' ', indent_count: 2)
  end

  def car(arr)
    case arr
    when Array 
      arr[0]
    when Cons
      arr.car
    end
  end

  def placeof_car(arr)
    case arr
    when Array
      proc { |v| arr[0] = v }
    when Cons
      proc { |v| arr.car = v }
    end
  end

  def cdr(arr)
    case arr
    when Array
      arr[1..-1]
    when Cons
      arr.cdr
    end
  end

  def placeof_cdr(arr)
    case arr
    when Array
      proc { |v| arr[1..-1] = v }
    when Cons
      proc { |v| arr.cdr = v }
    end
  end

  def max(*xs)
    xs.max
  end

  # けったいな関数
  def make_random_state(state = nil)
    case state
    when Random
      return state.dup
    when true
      return Random.new
    else
      return $random_state.dup
    end
  end

  def random(n)
    $random_state.rand(n)
  end

  def floor(n, divisor = 1)
    (n / divisor).floor
  end

  def make_array(arg)
    case arg
    when Integer
      Array.new(arg)
    when Array
      if arg.size == 0
        nil 
      else
        Array.new(arg[0]) { make_array(arg.drop(1)) }
      end
    end
  end

  def placeof_aref(arr, *indices)
    proc { |value|
      tmp = arr
      indices[0..-2].each do |i|
        tmp = tmp[i]
      end
      tmp[indices[-1]] = value
    }
  end

  def null(v)
    case v
    when nil, false, []
      return true
    else
      return false
    end
  end

  def length(arr)
    arr.size
  end

  def nth(index, arr)
    arr[index]
  end

  # def placeof_nth

  def remove(item, sequence)#, opts = {})
    return sequence.reject { |i| i == item }
  end

  def cadr(arr)
    arr[1]
  end

  def append(*lists)
    if lists.empty?
      nil
    else
      lists.inject(:+)
    end
  end

  def apply(f, args)
    f.call(*args)
  end

  def format(destination, control_string, *args)
    cs1 = control_string.gsub("~%", "\n").gsub("~a", "%s").gsub("~", "%")
    output = cs1 % args

    case destination
    when true
      STDOUT.write output
      return nil
    when nil
      return output
    else
      fail "format: unsupported destionation #{destination.inspect}"
    end
  end

  def fresh_line
    puts
    return true
  end

  def every(pred, list)
    list.all?(&pred)
  end

  def code_char(code)
    code.chr
  end

  def ash(integer, count)
    integer << count
  end

  def type_of(instance)
    if instance.respond_to?(:_type)
      instance._type
    else
      instance.class.to_sym.downcase
    end
  end

  def mod(number, divisor)
    number.divmod(divisor)[1]
  end

  def mapcar(function, *lists)
    shortest = lists.map(&:size).min
    shortest.times.map do |i|
      args = lists.map { |ls| ls[i] }
      function.call(*args)
    end
  end

  def plus(*operands)
    operands.inject(0, :+)
  end

  def less_than(*operands)
    operands.each_cons(2).all? { |a,b| a < b }
  end

  def numberp(v)
    v.is_a? Numeric
  end

  def integerp(v)
    v.is_a? Integer
  end

  def symbol_name(sym)
    sym.to_s.upcase # 大文字化すべきでない？
  end

  def char(str, index)
    str[index]
  end

  def char_code(c)
    c.ord
  end
end

class Object
  $cl_methods = Hash.new { [] }
  def defmethod(name, params, &body)
    unless self.respond_to?(name)
      define_method(name) do |*args|
        if args.size != params.size
          fail ArgumentError, "wrong number of arguments (given #{args.size}, expected #{params.size})"
        end

        applicable_methods = $cl_methods[name].select { |m|
          m[:param_types].zip(args).all? { |t,a| t === a } # applicable?
        }.sort_by { |m| # sort by specificity
          m[:param_types]
        }.map { |m| m[:proc] }

        make_chain = proc do |methods|
          if methods.empty?
            lambda { fail 'no next method' }
          else
            m = methods[0]
            tail = make_chain.call(methods[1..-1])
            lambda { m.call(args, tail) }
          end
        end
        make_chain.call(applicable_methods).call
      end
    end

    m = { param_types: params, proc: body }
    $cl_methods[name] += [m]
    return name
  end
end

class Cons
  class << self
    def [](car, cdr)
      Cons.new(car, cdr)
    end
  end

  attr_accessor :car, :cdr
  def initialize(car, cdr)
    @car = car
    @cdr = cdr
  end

  def inspect
    "(#{@car} . #{@cdr})"
  end
end
