class Translator
  def initialize
    @structs = {}
  end

  def translate_function_name(symbol)
    symbol1 = symbol.to_s
    if symbol1 =~ /:/
      package, fname = symbol1.split(':', 2)
      modname = sanitize_module_name(package)
      fname1 = sanitize_method_name(fname)
      return "#{modname}.#{fname1}"
    else
      return translate_symbol(symbol)
    end
  end

  def translate_function_application(name, args)
    fail "#{name.inspect} is not a symbol" unless name.is_a? Symbol
    args1 = translate_argument_list(args)
    return "#{translate_function_name(name)}(#{args1})"
  end

  def translate_defun(args)
    fail unless args.size >= 2
    fail unless args[0].is_a? Symbol
    fail unless args[1].is_a? Array

    name, params, *body = args

    b = "def #{translate(name)}("
    b += translate_parameter_list(params)
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
      .sub(/<=$/, '_less_eq')
      .sub(/>=$/, '_more_eq')
      .sub(/\/=$/, '_not_eq')
      .sub(/=$/, '_eq')
      .sub(/\?$/, 'p')
      .gsub(/\W/, '_')
  end

  def sanitize_module_name(name)
    name1 = name.gsub(/[^\w?]/, '_')

    if name1 !~ /^[A-Z]/
      name1.capitalize
    else
      name1
    end
  end

  def sanitize_method_name(name)
    name.gsub(/->/, '_to_')
      .gsub(/[^\w?]/, '_')
  end

  def translate_symbol(symbol)
    symbol1 = symbol.to_s
    case symbol1
    when "t"
      "true"
    when "nil"
      "nil"
    when /^:(.*)/ # keyword
      $1.to_sym.inspect
    when /^\*(.+)\*/ # earmuffed
      "$#{sanitize_varname($1)}"
    when /^\+(.+)\+/ # constant
      "#{sanitize_varname($1).upcase}"
    when /:/ # symbol in another package
      mod, sym = symbol1.split(':',2)
      sanitize_module_name(mod) + "::" + sanitize_varname(sym)
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
    case f
    when :+
      "method(:plus)"
    when :<
      "method(:less_than)"
    else
      f1 = translate_function_name(f)
      if f1 =~ /\./
        mod, fun = f1.split('.',2)
        "#{mod}.method(:#{fun})"
      else
        "method(#{f1.to_sym.inspect})"
      end
    end
  end

  def translate_argument_list(args)
    first_keyword = args.index { |a| a.is_a?(Symbol) && a.to_s[0] == ":" }
    if first_keyword
      normal_args = args[0...first_keyword]
      keyword_args = args[first_keyword..-1]

      if keyword_args.size.odd?
        normal_args = args
        keyword_args = []
      end
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

  def translate_parameter_list(ls)
    ls1 = ls.dup
    translated = []
    until ls1.empty?
      sexp = ls1.shift
      case sexp
      when :"&optional"
        until ls1.empty?
          pair = ls1.shift
          case pair
          when Symbol
            translated << "#{translate pair} = nil"
          when Array
            translated << "#{translate(pair[0])} = #{translate(pair[1])}"
          else
            fail
          end
        end
      when :"&rest"
        sexp = ls1.shift
        translated << "*#{translate(sexp)}"
        ls1.shift
      else
        translated << translate(sexp)
      end
    end
    return translated.join(', ')
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
      name = name[0]
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
    b += "def _type\n#{name.inspect}\nend\n"

    # コンストラクタ定義

    b += "def initialize(opts = {})\n"
    if superklass
      b += "super(opts)\n"
    end
    fields1.each do |fname, init|
      spec = "opts[#{fname.to_sym.inspect}]"
      b += "@#{fname} = #{spec}.nil? ? #{init} : #{spec}\n"
    end
    b += "end\n"

    b += "end\n" # class

    b += "def make_#{name1}(opts = {})\n"
    b += "#{name1.capitalize}.new(opts)\n"
    b += "end\n"

    # 型判別述語定義

    b += "def #{name1}_p(v)\n"
    b += "v.is_a?(#{name1.capitalize})\n"
    b += "end\n"

    # アクセッサー関数定義
    all_fields = @structs[name][:fields]
    all_fields.each do |fname, init|
      b += "def #{name1}_#{translate(fname)}(x)\nx.#{translate(fname)}\nend\n"
      b += "def placeof_#{name1}_#{translate(fname)}(x)\nproc { |v| x.#{translate(fname)} = v }\nend\n"
    end

    return b += "#{name1.to_sym.inspect}"
  end

  def translate_quote(params)
    fail unless params.size == 1
    sexp, = params

    case sexp
    when Array
      if sexp.size == 3 && sexp[1] == :"."
        "Cons[#{translate_quote([sexp[0]])}, #{translate_quote([sexp[2]])}]"
      else
        list = sexp.map { |e| translate_quote([e]) }.join(', ')
        "[#{list}]"
      end
    when :t
      "true"
    when :nil
      "nil"
    when Symbol
      sexp.inspect
    when Integer
      sexp.to_s
    when String
      sexp.inspect
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

  # def find_struct_accessor(funcname)
  #   funcname1 = funcname.to_s
  #   @structs.each do |name_sym, dict|
  #     dict[:fields].each do |fname_sym|
  #       if funcname1 == "#{name_sym}-#{fname_sym}"
  #         return p([name_sym, fname_sym])
  #       end
  #     end
  #   end
  #   return [nil,nil]
  # end

  def translate_place(exp)
    fail unless exp.is_a? Array
    fail unless exp.size > 0

    arglist = translate_argument_list(exp[1..-1])
    "placeof_#{translate(exp[0])}(#{arglist})"
  end

  def translate_setf(params)
    fail 'odd number of params' unless params.size.even?
    if params.size == 0
      return "nil"
    end

    translate_pair = lambda do |(lhs, rhs)|
      case lhs
      when Symbol
        "#{translate(lhs)} = #{translate(rhs)}"
      when Array
        "#{translate_place(lhs)}.(#{translate(rhs)})"
      end
    end

    return params.each_slice(2).map(&translate_pair).join("\n")
  end

  def translate_push(params)
    fail unless params.size == 2
    elt, ls = params

    translate_setf [ls, [:push_imp, elt, ls]]
    #"#{translate_place(ls)} = push_imp(#{translate(elt)}, #{translate(ls)})"
  end

  def translate_incf(params)
    lhs, amount = params
    amount ||= 1
    translate_setf [lhs, [:+, lhs, amount]]
    #"#{translate_place(lhs)} += #{translate(amount)}"
  end

  def translate_decf(params)
    lhs, amount = params
    amount ||= 1
    translate_setf [lhs, [:-, lhs, amount]]
    #"#{translate_place(lhs)} -= #{translate(amount)}"
  end

  def translate_case(params)
    test, *clauses = params
    b = "case #{translate(test)}\n"
    clauses.each do |val, *body|
      if val == :otherwise
        b += "else\n"
      elsif val.is_a? Array
        vals = val.map { |v| translate([:quote, v]) }.join(", ")
        b += "when #{vals}\n"
      else
        b += "when #{translate([:quote, val])}\n"
      end
      b += body.map(&method(:translate)).join("\n") + "\n"
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
    "lambda { |#{translate_argument_list(arglist)}|\n" +
      body.map(&method(:translate)).join("\n") + "\n}"
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
        ptypes << "Object"
      when Array
        name, type, = sexp
        pnames << name
        ptypes << translate_symbol(type).capitalize
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

  # analysis phase
  def translate_loop_analyze(input)
    body = []
    vars = []
    test = nil
    collect = nil # loop variable to be recorded
    test_polarity = true

    loop do
      if input.empty?
        break
      elsif input[0] == :do
        body = input[1..-1]
        input = []
      elsif input[0] == :for
        input.shift
        var = input[0]
        lbound = 0
        ubound = Float::INFINITY
        inclusive = false
        done = false
        input.shift
        while true
          case input[0]
          when :from
            input.shift
            lbound = input[0]
            input.shift
          when :below
            input.shift
            ubound = input[0]
            inclusive = false
            input.shift
          when :to
            input.shift
            ubound = input[0]
            inclusive = true
            input.shift
          when :in
            input.shift
            vars << { name: var, type: :list, list: input[0] }
            input.shift
            done = true
            break
          when :across
            input.shift
            vars << { name: var, type: :list, list: input[0] }
            input.shift
            done = true
            break
          when :"="
            input.shift
            init = input.shift
            if input[0] == :then
              input.shift
              then_exp = input.shift
            else
              then_exp = var
            end
            vars << { name: var, type: :then, init: init, then: then_exp }
            done = true
          else
            break
          end
        end
        unless done
          vars << { name: var, type: :range, lbound: lbound, ubound: ubound, inclusive: inclusive }
        end
      elsif input[0] == :while
        input.shift
        test = input[0]
        input.shift
      elsif input[0] == :until
        input.shift
        test = input[0]
        test_polarity = false
        input.shift
      elsif input[0] == :collect
        input.shift
        collect = input[0]
        input.shift
      else
        fail "unexpected token #{input[0]}"
      end
    end
    return { body: body, vars: vars, test: test, collect: collect, test_polarity: test_polarity }
  end

  # generation phase
  def translate_loop_generate(body:, vars:, test:, collect:, test_polarity:)
    b = ""
    b += "progn do\n" # contain generators
    vars.each do |v|
      case v[:type]
      when :range, :list
        b += "#{translate(v[:name])}_gen = Enumerator.new do |y|\n"
        case v[:type]
        when :range
          op =  v[:inclusive] ? ".." : "..."
          ubound1 = (v[:ubound] == Float::INFINITY) ? "Float::INFINITY" : translate(v[:ubound])
          b += "(#{translate v[:lbound]} #{op} #{ubound1}).each do |e|\n"
        when :list
          b += "(#{translate v[:list]} || []).each do |e|\n"
        else fail
        end
        b += "y << e\n"
        b += "end\n" # each
        b += "end\n" # Enumerator.new
      when :then
        b += "#{translate(v[:name])} = #{translate(v[:init])}\n"
      else
        fail
      end
    end
    if collect
      b += "#{translate collect}_collected = []\n"
    end

    b += "begin\n"
    if test
      if test_polarity
        b += "while #{translate(test)}\n"
      else
        b += "until #{translate(test)}\n"
      end
    else
      b += "loop do\n"
    end
    vars.each do |v|
      case v[:type]
      when :range, :list
        b += "#{translate v[:name]} = #{translate v[:name]}_gen.next\n"
      end
    end
    b += "progn do\n"
    body.each do |sexp|
      b += translate(sexp) + "\n"
    end
    b += "end\n" # progn
    b += "#{translate collect}_collected << #{translate collect}\n" if collect

    vars.each do |v|
      case v[:type]
      when :then
        b += "#{translate(v[:name])} = #{translate(v[:then])}\n"
      end
    end

    b += "end\n" # loop
    b += "rescue StopIteration\n"
    b += "end\n" # begin
    if collect
      b += "#{translate collect}_collected\n"
    else
      # b += "nil\n"
    end
    return b += "end" # progn
  end

  def translate_loop(params)
    return translate_loop_generate translate_loop_analyze(params.dup)
  end

  def translate_flet(params)
    defuns, *body = params

    b = ""
    defuns.each do |name, arglist, *fbody|
      b += "define_method(#{ translate(name).to_sym.inspect }) do |#{ translate_argument_list(arglist) }|\n"
      b += fbody.map(&method(:translate)).join("\n") + "\n"
      b += "end\n"
    end

    return b += body.map(&method(:translate)).join("\n")
  end

  def translate_labels(params)
    translate_flet(params)
  end

  def translate_destructuring_bind(params)
    vars, init, *body = params
    b = ""
    b += "progn do\n"
    b += "#{translate_argument_list(vars)} = #{translate init}\n"
    b += body.map(&method(:translate)).join("\n")
    return b += "\nend"
  end

  def translate_with_open_file(params)
    (var, *args), *body = params
    b = "with_open_file(#{translate_argument_list(args)}) do |#{translate var}|\n"
    b += body.map(&method(:translate)).join(", ")
    return b += "\nend"
  end

  def translate_dolist(params)
    (var, list), *body = params # result-form is not supported
    b = "dolist(#{translate(list)}) do |#{translate(var)}|\n"
    b += body.map(&method(:translate)).join("\n") + "\n"
    return b += "end"
  end

  def translate_load(params)
    params += [:":from", :__FILE__]
    return translate_function_application(:load, params)
  end

  def translate(sexp)
    case sexp
    when Symbol
      translate_symbol(sexp)
    when String
      "#{sexp.inspect}"
    when Array
      if sexp.empty?
        "nil"
      # elsif sexp.size == 3 && sexp[1] == :"."
      #   translate_dot_pair(sexp[0], sexp[2])
      else
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
        when :loop
          translate_loop(rest)
        when :flet
          translate_flet(rest)
        when :push
          translate_push(rest)
        when :load
          translate_load(rest)
        when :labels
          translate_labels(rest)
        when :"destructuring-bind"
          translate_destructuring_bind(rest)
        when :"with-open-file"
          translate_with_open_file(rest)
        when :"call-next-method"
          translate_call_next_method(rest)
        when :"dolist"
          translate_dolist(rest)
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
      end
    when Integer
      "#{sexp.inspect}"
    else
      fail "case not covered for #{sexp.inspect}"
    end
  end
end
