module Reader
  MACRO_CHARS = {}
  MACROS = {}

  module_function

  def compile(ls)
    ls.map { |sexp| compile_sexp(sexp) }.join("\n")
  end

  def compile_lambda_list(ls)
    if (i = ls.index(:"&rest")) != nil
      ls.delete(:"&rest")
    end
    result = ls.map do |item|
      if item.is_a? Array
        "(#{compile_lambda_list(item)})"
      else
        item.to_s
      end
    end
    if i
      result[i] = "*" + result[i]
    end
    result.join(', ')
  end

  def arglist(list)
    if list.empty?
      ""
    else
      "(" + list.join(', ') + ")"
    end
  end

  def macroexpand(sexp)
    if not sexp.is_a? Array # if atom
      sexp
    elsif sexp.empty?
      []
    elsif MACROS.has_key?(sexp[0])
      macro = MACROS[sexp[0]]
      macroexpand macro.call(*sexp[1..-1])
    else
      [sexp[0]] + sexp[1..-1].map(&method(:macroexpand))
    end
  end

  class CloseParenException < StandardError
    attr_reader :rest
    def initialize(rest)
      @rest = rest
    end
  end

  ORDINARY_CHARS = ((32..126).map(&:chr) - [" ", ?#, ?(, ?), ?', ?", ?;]).join

  class PrematureEndException < StandardError
    attr_reader :rest

    def initialize(msg, rest)
      super(msg)
      @rest = rest
    end
  end

  class NullInputException < StandardError
  end

  class NoSexpException < StandardError
    attr_reader :rest
    def initialize(rest)
      @rest = rest
    end
  end

  # String -> Object
  def read_sexp(input)
    sexp, = read(input)
    sexp
  end

  def read_all(input)
    text = []
    rest = input
    loop do
      sexp, rest = read(rest)
      text << sexp
    end
    fail 'logic error?'
  rescue NullInputException
    return text
  end

  # String -> [Object, String]
  def read input
    # 動いてるけど読めなさすぎる
    begin
      unless MACRO_CHARS.empty?
        if input =~ /\A([#{Regexp.escape(MACRO_CHARS.keys.join)}])/
          char1 = $1.upcase
          rest = $'
          if MACRO_CHARS[char1].respond_to? :call
            return MACRO_CHARS[char1].call(rest, char1)
          elsif (ftable = MACRO_CHARS[char1]).is_a? Hash
            char2 = rest[0].upcase
            if fn = ftable[char2]
              return fn.call(rest[1..-1], char1, char2)
            else
              raise "no dispatch macro for #{char1} #{char2}"
            end
          else
            raise "something wrong"
          end
        end
      end
    rescue NoSexpException => e
      input = e.rest
      retry
    end

    case input
    when ''
      raise NullInputException, 'null input'
    when /\A\s+/
      read $'
    when /\A\)/
      raise CloseParenException.new($')
    when /\A\(/
      begin
        str = $'
        result = []
        loop do
          sexp, str = read(str)
          result << sexp
        end
      rescue CloseParenException => e
        return [result, e.rest]
      rescue NullInputException => e
        raise PrematureEndException.new("expecting )", str)
      end
      raise 'unreachable'
    when /\A[#{Regexp.escape ORDINARY_CHARS}]+/
      rest = $'
      symbol_string = $&
      if symbol_string =~ /\A-?\d+\z/
        [symbol_string.to_i, rest]
      else
        [symbol_string.to_sym, rest]
      end
    when /\A"/
      rest = $'
      if ( match = rest.match(/\A[^"]*"/) ) != nil
        str = match.to_s[0..-2]
      else
        raise PrematureEndException, 'could not find balancing double quote'
      end
      [str, $']
    else
      raise "parse error #{input.inspect}"
    end
  end

  def set_macro_character char, fn
    MACRO_CHARS[char] = fn
  end

  def set_dispatch_macro_character char1, char2, fn
    MACRO_CHARS[char1] ||= char1
    MACRO_CHARS[char1][char2] = fn
  end

  def shift!(str)
    str[0].tap do
      str.sub!(/\A./m,'')
    end
  end

  MACRO_CHARS['#'] = {}

  # コメント記法
  set_macro_character(';', lambda { |input, char|
                        read(input.sub(/.*$/, ''))
                      })

  # 正規表現リテラル
  # set_macro_character('/', lambda { |input, char|
  #                       input = input.dup
  #                       buf = ""
  #                       while (char = shift!(input)) != '/'
  #                         if char == nil
  #                           raise PrematureEndException, "no balancing /"
  #                         elsif char == '\\' and ['/','\\'].include? input.first
  #                           buf << shift!(input)
  #                         else
  #                           buf << char
  #                         end
  #                       end
  #                       [Regexp.new(buf), input]
  #                     })

  # # インスタンスメソッド記法
  # set_macro_character('.', lambda { |input, char|
  #                       msg, rest = read(input)
  #                       exp = [:meth, msg]
  #                       [exp, rest]
  #                     })

  # ブロック引数記法
  set_dispatch_macro_character('#', '&',
                               lambda { |input, char1, char2|
                                 sexp, rest = read(input)
                                 [
                                  [:barg, [:function, sexp]], rest]
                               })

  # クォート
  set_macro_character("'",
                      lambda { |input, char|
                        exp, rest = read(input)
                        [[:quote, exp], rest]
                      })

  # #| ... |# 形式のコメント。本当はネストできるように作らないといけな
  # い。
  set_dispatch_macro_character("#", "|",
                               lambda { |input, _char_a, _char_b|
                                 input = input.dup
                                 until input.empty?
                                   if input[0] == '|'
                                     shift!(input)
                                     if input[0] == '#'
                                       raise NoSexpException.new(input[1..-1])
                                     end
                                   end
                                   shift!(input)
                                 end
                                 fail PrematureEndException, "no balancing |#"
                               })

  # 文字リテラル
  set_dispatch_macro_character("#", "\\",
                               lambda { |input, _char_a, _char_b|
                                 [input[0], input[1..-1]]
                               })

  # 16進リテラル
  set_dispatch_macro_character("#", "X",
                               lambda { |input, _char_a, _char_b|
                                 if input =~ /\A([a-fA-F0-9]+)/
                                   [$1.to_i(16), $']
                                 else
                                   fail 'invalid hex literal'
                                 end
                               })

  # 8進リテラル
  set_dispatch_macro_character("#", "O",
                               lambda { |input, _char_a, _char_b|
                                 if input =~ /\A([0-7]+)/
                                   [$1.to_i(8), $']
                                 else
                                   fail 'invalid octal literal'
                                 end
                               })

  # 関数名
  set_dispatch_macro_character("#", "\'",
                               lambda { |input, _char_a, _char_b|
                                 exp, rest = read(input)
                                 [[:function, exp], rest]
                               })

  # read-time 条件
  set_dispatch_macro_character("#", "+",
                               lambda { |input, _char_a, _char_b|
                                 feature, rest = read(input)
                                 exp, rest1 = read(rest)
                                 [:nil, rest1] # 本来空白になるべき
                               })

  read_infix_digits = lambda { |input, _hash, first_digit|
    input = input.dup
    digits = first_digit
    while input[0] =~ /\d/
      digits.concat(input[0])
      shift!(input)
    end
    case input[0]
    when 'A', 'a' # 配列リテラル
      shift!(input)
      list, rest = read(input)
      fail unless list.is_a? Array
      dimensions = []
      l = list
      digits.to_i.times do
        dimensions << l.size
        l = l[0]
      end
      [[:"make-array", [:quote, dimensions], :":initial-contents", [:quote, list]], rest]
    when 'R', 'r' # n 進数
      fail 'R unsupported'
    else
      fail "#{input[0].inspect}"
    end
  }

  ("0".."9").each do |c|
    set_dispatch_macro_character("#", c, read_infix_digits)
  end
end
