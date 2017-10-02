module Format
  module_function

  def group_digits(str, comma, interval)
    str.reverse.scan(/.{1,#{interval}}/).join(comma).reverse
  end

  def princ_format(value, nil_as_list)
    case value
    when String
      s = value
    when Symbol
      s = value.to_s
    when nil
      s = nil_as_list ? "()" : "nil"
    when Array
      s = "(" + value.map { |v| princ_format(v, nil_as_list) }.join(' ') + ")"
    else
      s = value.inspect
    end
    return s
  end

  def prin1_format(value, nil_as_list)
    case value
    when String
      s = value.inspect
    when Symbol
      s = value.to_s
    when nil
      s = nil_as_list ? "()" : "nil"
    when Array
      s = "(" + value.map { |v| prin1_format(v, nil_as_list) }.join(' ') + ")"
    else
      s = value.inspect
    end
    return s
  end

  def aesthetic(value, mincol, colinc, minpad, padchar, at, colon)
    s = nil
    padchar ||= ' '
    colinc ||= 1
    s = princ_format(value, colon)
    if minpad
      minpad.times do
        if at
          s = padchar + s
        else
          s = s + padchar
        end
      end
    end
    if mincol
      until s.size >= mincol
        if at
          s = (padchar * colinc) + s
        else
          s = s + (padchar * colinc)
        end
      end
    end
    return s
  end

  def standard(value, mincol, colinc, minpad, padchar, at, colon)
    s = nil
    padchar ||= ' '
    colinc ||= 1
    s = prin1_format(value, colon)
    if minpad
      minpad.times do
        if at
          s = padchar + s
        else
          s = s + padchar
        end
      end
    end
    if mincol
      until s.size >= mincol
        if at
          s = (padchar * colinc) + s
        else
          s = s + (padchar * colinc)
        end
      end
    end
    return s
  end

  def dispatch(char, args, values, at, colon)
    case char.upcase
    when 'D'
      value = values.shift
      mincol, padchar, commachar, comma_interval = args
      padchar ||= ' '
      signed = at

      s = value.to_s
      if colon
        s = group_digits(s, commachar, comma_interval)
      end
      if value >= 0 && signed # 0 → "+0"
        s = '+' + s
      end
      if mincol && s.size < mincol
        s = (padchar * (mincol - s.size)) + s
      end
      return s
    when 'C'
      value = values.shift
      fail "unimplemented" if at || colon
      if value.size > 1
        fail 'not a character'
      end
      return value
    when 'A'
      value = values.shift
      return aesthetic(value, args[0], args[1], args[2], args[3], at, colon)
    when 'S'
      value = values.shift
      return standard(value, args[0], args[1], args[2], args[3], at, colon)
    when '~'
      return '~'
    when '%'
      return "\n"
    else
      fail char.upcase
    end
  end

  def format(cs, *values)
    values = values.dup
    input = cs.chars
    output = ""

    until input.empty?
      c = input.shift
      case c
      when '~'
        colon = false
        at = false
        args = []
        control_char = nil

        until input.empty?
          c = input.shift
          case c
          when ':'
            colon = true
          when '@'
            at = true
          when /\d/
            digits = c

            until input.empty?
              c = input.shift
              if c =~ /\d/
                digits.concat(c)
              elsif c == ','
                break
              else
                input.unshift c
                break
              end
            end

            args << digits.to_i
          when '\''
            fail 'quoted char expected' if input.empty?
            args << input.shift
            c = input.shift
            if c != ','
              input.unshift c
            end
          when ','
            args << nil
          else
            control_char = c
            break
          end
        end

        output.concat dispatch(control_char, args, values, at, colon)
      else
        output.concat(c)
      end
    end

    return output
  end

  fail unless "ABCpqr   123   xyz" == Format.format("ABC~,,3A123~,,3@A", "pqr", "xyz")
  fail unless "(a B)-----" == Format.format("~10,,,'-A", [:a, "B"])
  fail unless "ABCpqr@@@123***xyz" == Format.format("ABC~,,3,'@A123~,,3,'*@A", "pqr", "xyz")
  fail unless "(a B)--------" == Format.format("~10,4,,'-A", [:a, "B"])
  fail unless "-----(a B)" == Format.format("~10,4,1,'-@A", [:a, "B"])

  fail unless "----(a \"B\")" == Format.format("~10,4,,'-@S", [:a, "B"])
end
