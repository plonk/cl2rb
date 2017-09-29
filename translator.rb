class Translator
  def initialize
    @structs = {}
  end

  def translate_function_application(name, args)
    fail "#{name.inspect} is not a symbol" unless name.is_a? Symbol
    args1 = translate_argument_list(args)
    return "#{translate(name)}(#{args1})"
  end

  def translate_params(params)
    params.join(', ')
  end

  def translate_defun(args)
    fail unless args.size >= 2
    fail unless args[0].is_a? Symbol
    fail unless args[1].is_a? Array

    name, params, *body = args

    b = "def #{translate(name)}("
    b += translate_params(params)
    b += ")\n"
    body.each do |sexp|
      b += translate(sexp)
      b += "\n"
    end
    return b += "end"
  end

  def translate_if(args)
    cond, then_clause, else_clause = args
    fail 'condition' unless cond
    fail 'then clause' unless then_clause

    b = "if "
    b += translate(cond) + "\n"
    b += translate(then_clause) + "\n"
    if else_clause
      b += "else\n"
      b += translate(else_clause) + "\n"
    end
    return b += "end"
  end

  def sanitize_varname(name)
    name.gsub(/->/, '_to_')
      .sub(/\?$/, 'p')
      .gsub(/\W/, '_')
  end

  def translate_symbol(symbol)
    symbol1 = symbol.to_s
    case symbol1
    when "t"
      "true"
    when /^:(.*)/ # keyword
      $1.to_sym.inspect
    when /^\*(.+)\*/ # earmuffed
      "$#{sanitize_varname($1)}"
    when /^\+(.+)\+/ # constant
      "#{sanitize_varname($1).upcase}"
    else
      sanitize_varname(symbol1)
    end
  end

  def translate_equal(_op, es)
    es1 = es.map(&method(:translate))
    es1.each_cons(2).map { |a,b|
      "#{a} == #{b}"
    }.join(" && ")
  end

  def translate_numerical_comparison_operator(op, es)
    es1 = es.map(&method(:translate))
    es1.each_cons(2).map { |a,b|
      "#{a} #{op} #{b}"
    }.join(" && ")
  end

  # オペランドが2個未満だった時に動かない
  def translate_arithmetic_operator(op, operands)
    operands1 = operands.map(&method(:translate))
    operands1.join(" #{op} ")
  end

  def translate_one_minus(args)
    fail unless args.size == 1
    arg, = args
    "(#{translate(arg)} - 1)"
  end

  def translate_one_plus(args)
    fail unless args.size == 1
    arg, = args
    "(#{translate(arg)} + 1)"
  end

  def translate_defparameter(params)
    name, initial_value, = params
    "#{translate_symbol(name)} = #{translate(initial_value)}"
  end

  def translate_dotimes(params)
    (var, ulimit), *body = params
    "#{translate(ulimit)}.times do |#{translate(var)}|\n" +
      body.map(&method(:translate)).join("\n") + "\nend"
  end

  def translate_function(params)
    fail unless params.size == 1
    fail unless params[0].is_a? Symbol
    f, = params
    "method(#{translate(f).to_sym.inspect})"
  end

  def translate_argument_list(args)
    first_keyword = args.index { |a| a.is_a?(Symbol) && a.to_s[0] == ":" }
    if first_keyword
      normal_args = args[0...first_keyword]
      keyword_args = args[first_keyword..-1]
    else
      normal_args = args
      keyword_args = []
    end

    normal_args_translated = normal_args.map(&method(:translate))
    keyword_args_translated = keyword_args.each_slice(2).map { |k, v|
      # ややこしい…
      k.to_s.sub(/^:/,'').to_sym.inspect.sub(/^:/,'') + ": " + translate(v)
    }

    return (normal_args_translated + keyword_args_translated).join(', ')
  end

  def translate_funcall(params)
    f, *args = params
    arglist = translate_argument_list(args)
    return "#{translate(f)}.call(#{arglist})"
  end

  def find_superklass(defstruct_options)
    inc = defstruct_options.each_slice(2).to_a.assoc(:":include")
    inc && inc[1]
  end

  def register_struct(name, own_fields, superklass)
    superfields = superklass ? @structs[superklass][:fields] : []
    @structs[name] = { fields: superfields + own_fields }
  end

  def translate_defstruct(params)
    fail 'defstruct' unless params.size >= 1
    name, *fields = params
    case name
    when Symbol
      name1 = translate_symbol(name)
    when Array
      name1 = translate_symbol(name[0])
      superklass = find_superklass(name[1])
    end
    fail 'invalid field definition' unless fields.all? { |f| f.is_a? Array }
    fail 'invalid field definition' unless fields.all? { |f| f.size == 2 }
    fields1 = fields.map { |fname, init| [translate(fname), translate(init)] }

    register_struct(name, fields.map(&:first), superklass)

    # クラス定義
    b = ""
    if superklass
      b += "class #{name1.capitalize} < #{translate(superklass).capitalize}\n"
    else
      b += "class #{name1.capitalize}\n"
    end
    fields1.each do |fname, _init|
      b += "attr_accessor #{fname.to_sym.inspect}\n"
    end

    # コンストラクタ定義

    b += "def initialize(opts = {})\n"
    if superklass
      b += "super(opts)\n"
    end
    fields1.each do |fname, _init|
      b += "self.#{fname} = (tmp = opts[#{fname.to_sym.inspect}]).nil? ? #{_init} : tmp\n"
    end
    b += "end\n"

    b += "end\n" # class

    b += "def make_#{name1}(opts = {})\n"
    b += "#{name1.capitalize}.new(opts)\n"
    b += "end\n"


    # アクセッサー関数定義
    all_fields = @structs[name][:fields]
    all_fields.each do |fname, init|
      b += "def #{name1}_#{translate(fname)}(x); x.#{translate(fname)}; end\n"
    end

    return b
  end

  def translate_quote(params)
    fail unless params.size == 1
    sexp, = params

    case sexp
    when Array
      "[#{sexp.map(&method(:translate)).join(', ')}]"
    when Symbol
      sexp.inspect
    when Integer
      sexp.to_s
    else
      fail "unhandled case #{sexp.inspect}"
    end
  end

  def translate_let(params)
    fail unless params.size >= 1
    fail unless params[0].is_a?(Array) && params[0].all? { |pair| pair.is_a?(Array) && pair.size == 2 }

    assign_list, *body = params
    vars, values = assign_list.transpose

    vars1 = vars.map(&method(:translate)).join(', ')
    values1 = values.map(&method(:translate)).join(', ')

    b = ""
    b += "let(#{values1}) do |#{vars1}|\n"
    body.each do |sexp|
      b += translate(sexp) + "\n"
    end
    return b += "end"
  end

  def translate_let_star(params)
    fail unless params.size >= 1
    fail unless params[0].is_a?(Array) && params[0].all? { |pair| pair.is_a?(Array) && pair.size == 2 }

    assign_list, *body = params

    b = "progn do\n"
    assign_list.each do |var, init|
      b += "#{translate(var)} = #{translate(init)}\n"
    end
    b += "\n"
    body.each do |sexp|
      b += translate(sexp) + "\n"
    end
    return b += "end"
  end

  def translate_place(exp)
    if exp.is_a? Symbol
      return translate(exp)
    end

    fail unless exp.is_a? Array
    fail unless exp.size >= 2

    case exp[0]
    when :aref
      translate_aref(exp[1..-1])
    when /^(.+?)-(.+)$/ # assume $1 is a type name
      "(#{translate(exp[1])}).#{translate($2.to_sym)}"
    else fail "#{exp[0]} unimplemented"
    end
  end

  def translate_setf(params)
    fail 'odd number of params' unless params.size.even?
    if params.size == 0
      return "nil"
    end

    translate_pair = lambda do |(lhs, rhs)|
      "#{translate_place(lhs)} = #{translate(rhs)}"
    end

    return params.each_slice(2).map(&translate_pair).join("\n")
  end

  def translate_incf(params)
    lhs, amount = params
    amount ||= 1
    "#{translate_place(lhs)} += #{translate(amount)}"
  end

  def translate_decf(params)
    lhs, amount = params
    amount ||= 1
    "#{translate_place(lhs)} -= #{translate(amount)}"
  end

  def translate_case(params)
    test, *clauses = params
    b = "case #{translate(test)}\n"
    clauses.each do |val, body|
      if val == :otherwise
        b += "else\n"
      else
        b += "when #{translate([:quote, val])}\n"
      end
      b += "#{translate(body)}\n"
    end
    return b += "end"
  end

  def translate_when(params)
    cond, *body = params
    b = "if "
    b += translate(cond) + "\n"
    body.each do |sexp|
      b += translate(sexp) + "\n"
    end
    return b += "end"
  end

  def translate_cond(params)
    b = ""
    params.each.with_index do |(test, *body), i|
      if i == 0
        b += "if #{translate(test)}\n"
      else
        b += "elsif #{translate(test)}\n"
      end
      b += body.map(&method(:translate)).join("\n") + "\n"
    end
    return b += "end"
  end

  def translate_unless(params)
    test, *body = params
    b = ""
    b += "unless #{translate(test)}\n"
    b += body.map(&method(:translate)).join("\n") + "\n"
    return b += "end"
  end

  def translate_or(params)
    return "(" + params.map(&method(:translate)).join(' || ') + ")"
  end

  def translate_and(params)
    return "(" + params.map(&method(:translate)).join(' && ') + ")"
  end

  def translate_lambda(params)
    arglist, *body = params
    "lambda do |#{translate_argument_list(arglist)}|\n" +
      body.map(&method(:translate)).join("\n") + "\nend"
  end

  def translate_progn(params)
    params.map(&method(:translate)).join("\n")
  end

  def translate_aref(params)
    array, *indices = params
    "#{translate(array)}" + indices.map { |i| "[#{translate(i)}]" }.join
  end

  def translate_defmethod(params)
    funcname, paramlist, *body = params

    pnames = []
    ptypes = []
    paramlist.each do |sexp|
      case sexp
      when Symbol
        pnames << sexp
        ptypes << Object
      when Array
        name, type, = sexp
        pnames << name
        ptypes << type.to_s.capitalize
      end
    end

    pnames1 = pnames.map(&method(:translate)).join(', ')
    ptypes1 = ptypes.join(', ')
    b = "defmethod #{translate(funcname).to_sym.inspect}, [#{ptypes1}] do |(#{pnames1}), next_method|\n"
    b += body.map(&method(:translate)).join("\n")
    return b += "\nend"
  end

  def translate_call_next_method(params)
    fail unless params.empty?
    "next_method.call"
  end

  def translate(sexp)
    case sexp
    when Symbol
      translate_symbol(sexp)
    when String
      "#{sexp.inspect}"
    when Array
      if sexp.size >= 1
        first, *rest = sexp
        case first
        when :defun
          translate_defun(rest)
        when :if
          translate_if(rest)
        when :defparameter
          translate_defparameter(rest)
        when :defconstant
          translate_defparameter(rest)
        when :setf
          translate_setf(rest)
        when :function
          translate_function(rest)
        when :funcall
          translate_funcall(rest)
        when :defstruct
          translate_defstruct(rest)
        when :quote
          translate_quote(rest)
        when :let
          translate_let(rest)
        when :"let*"
          translate_let_star(rest)
        when :sef
          translate_setf(rest)
        when :incf
          translate_incf(rest)
        when :decf
          translate_decf(rest)
        when :case
          translate_case(rest)
        when :when
          translate_when(rest)
        when :cond
          translate_cond(rest)
        when :unless
          translate_unless(rest)
        when :and
          translate_and(rest)
        when :or
          translate_or(rest)
        when :dotimes
          translate_dotimes(rest)
        when :lambda
          translate_lambda(rest)
        when :progn
          translate_progn(rest)
        when :aref
          translate_aref(rest)
        when :defmethod
          translate_defmethod(rest)
        when :"call-next-method"
          translate_call_next_method(rest)
        when :"="
          translate_equal(first, rest)
        when :"*", :"+", :"-", :"/"
          translate_arithmetic_operator(first, rest)
        when :"<", :"<=", :">", :">="
          translate_numerical_comparison_operator(first, rest)
        when :"1-"
          translate_one_minus(rest)
        when :"1+"
          translate_one_plus(rest)
        else
          translate_function_application(first, rest)
        end
      else
        fail 'case not covered'
      end
    when Integer
      "#{sexp.inspect}"
    else
      fail "case not covered for #{sexp}"
    end
  end
end
