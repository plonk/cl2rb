load './cl2rb'

fail unless "0 == n" == translate_equal(:"=", [0, :n])
fail unless "x == y && y == z" == translate_equal(:"=", [:x, :y, :z])
