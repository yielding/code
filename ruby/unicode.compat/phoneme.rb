class Numeric
  def consonant?
    (0x3131..0x314e).include?(self)
  end

  def vowel?
    (0x314f..0x3163).include?(self)
  end
end

class Phoneme
  def initialize
    @consonant ||= {
      # compat    cho     jong
      0x3131 => [0x1100, 0x11A8],  # ㄱ
      0x3132 => [0x1101, 0x11A9],  # ㄲ
      0x3133 => [0x0000, 0x11AA],  # ㄱㅅ
      0x3134 => [0x1102, 0x11AB],  # ㄴ
      0x3135 => [0x0000, 0x11AC],  # ㄴㅈ
      0x3136 => [0x0000, 0x11AD],  # ㄴㅎ
      0x3137 => [0x1103, 0x11AE],  # ㄷ
      0x3138 => [0x1104, 0x1104],  # ㄸ
      0x3139 => [0x1105, 0x11AF],  # ㄹ
      0x313A => [0x11B0, 0x11B0],  # ㄹ
      0x313B => [0x0000, 0x11B1],  # ㄹㅁ
      0x313C => [0x0000, 0x11B2],  # ㄹㅂ
      0x3141 => [0x1106, 0x11B7],  # ㅁ
      0x3142 => [0x1107, 0x11B8],  # ㅂ
      0x3143 => [0x1108, 0x0000],  # ㅃ
      0x3145 => [0x1109, 0x11BA],  # ㅅ
      0x3146 => [0x110A, 0x11BB],  # ㅆ
      0x3147 => [0x110B, 0x11BC],  # ㅇ
      0x3148 => [0x110C, 0x11BD],  # ㅈ
      0x3149 => [0x110D, 0x0000],  # ㅉ
      0x314A => [0x110E, 0x11BE],  # ㅊ
      0x314B => [0x110F, 0x11BF],  # ㅋ
      0x314C => [0x1110, 0x11C0],  # ㅌ
      0x314D => [0x1111, 0x11C1],  # ㅍ
      0x314E => [0x1112, 0x11C2]   # ㅎ
    }
  end

  # 3개 연속 자음이 나오는 경우
  def compact_consonant(codes)
    i, result = 0, []

    while i < codes.length - 2
      first, second, third = codes[i], codes[i+1], codes[i+2]

      incr = 1
      if [first, second, third].all? { |e| e.consonant? }
        if first == 0x3131                     # ㄱ
          case second
          when 0x3145 then result.push(0x3133); incr = 2 # ㄳ
          else             result.push(first)
          end
        elsif first == 0x3134                  # ㄴ
          case second
          when 0x3148 then result.push(0x3135); incr = 2 # ㄵ
          when 0x314E then result.push(0x3136); incr = 2 # ㄵ
          else             result.push(first)
          end
        elsif first == 0x3139                  # ㄹ
          case second
          when 0x3131 then result.push(0x3162); incr = 2 # ㄺ
          when 0x3141 then result.push(0x313B); incr = 2 # ㄻ 
          when 0x3142 then result.push(0x313C); incr = 2 # ㄼ 
          when 0x3145 then result.push(0x313D); incr = 2 # ㄽ
          when 0x314C then result.push(0x313E); incr = 2 # ㄾ
          when 0x314D then result.push(0x313F); incr = 2 # ㄿ
          else             result.push(first)
          end
        else
          result.push(first)
        end
      else
        result.push(first)
      end

      i += incr
    end

    return result + codes.last(2)
  end

  def compact_vowels(codes)
    i, incr, result = 0, 0, []

    while i < codes.length - 1
      first, second = codes[i], codes[i+1]
      incr = 1
      if first == 0x3157                     # ㅗ
        case second
        when 0x314F then result.push(0x3158); incr = 2 # ㅘ
        when 0x3150 then result.push(0x3159); incr = 2 # ㅙ
        when 0x3163 then result.push(0x315A); incr = 2 # ㅚ
        else             result.push(first)
        end
      elsif first == 0x315C                  # ㅜ
        case second
        when 0x3153 then result.push(0x315D); incr = 2 # ㅝ
        when 0x3154 then result.push(0x315E); incr = 2 # ㅞ
        when 0x3163 then result.push(0x315F); incr = 2 # ㅟ
        else             result.push(first)
        end
      elsif first == 0x3161                  # ㅡ
        case second
        when 0x3163 then result.push(0x3162); incr = 2 # ㅢ
        else             result.push(first)
        end
      else
        result.push(first)
      end

      i += incr
    end

    result.push(codes.last) if incr == 1

    return result
  end

  def to_jamo(compat_jamo)
    res = compat_jamo.each_cons(2).map do |e|
      pos = index_of(e[0], e[1])
      translate(e[0], pos)
    end

    # special treat for the last char
    pos = index_of(compat_jamo.last, res.last)
    res.push(translate(compat_jamo.last, pos))
    return res
  end

  def translate(code, pos=0)
    return code if (0x3131..0x314e).include?(code) and pos > 1

    case code
    when 0x3131..0x314e then @consonant[code][pos]
    when 0x314f..0x3163 then code - 8174
    else
      code
    end
  end

private
  def index_of(c1, c2)
    return 100 if c1 == c2
    return 100 if c1.consonant? and (not c2.consonant? and not c2.vowel?)
    return 1   if c1.consonant? and c2.consonant?
    0
  end

end

=begin rdoc
@vowel ||= {
  # compat
  0x314F => 0x1161,  # ㅏ
  0x3150 => 0x1162,  # ㅐ 
  0x3151 => 0x1163,  # ㅑ
  0x3152 => 0x1164,  # ㅒ
  0x3153 => 0x1165,  # ㅓ
  0x3154 => 0x1166,  # ㅔ
  0x3155 => 0x1167,  # ㅕ
  0x3156 => 0x1168,  # ㅖ
  0x3157 => 0x1169,  # ㅗ
  0x3158 => 0x116A,  # ㅘ
  0x3159 => 0x116B,  # ㅙ
  0x315A => 0x116C,  # ㅚ
  0x315B => 0x116D,  # ㅛ
  0x315C => 0x116E,  # ㅜ
  0x315D => 0x116F,  # ㅝ
  0x315E => 0x1170,  # ㅞ
  0x315F => 0x1171,  # ㅟ
  0x3160 => 0x1172,  # ㅠ 
  0x3161 => 0x1173,  # ㅡ
  0x3162 => 0x1174,  # ㅢ
  0x3163 => 0x1175   # ㅣ
}
=end
