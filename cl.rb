require 'sxp'

# CommonLisp の関数を実装するモジュール
module CL
  module_function 

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
  end

  def get_internal_real_time
    Time.to_i * 1000
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
end

class Object
  $cl_methods = Hash.new { [] }
  def defmethod(name, params, &body)
    unless self.respond_to?(name)
      define_method(name) do |*args|
        if args.size != params.size
          fail ArgumentError "wrong number of arguments (given #{args.size}, expected #{params.size})"
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
