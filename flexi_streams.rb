module Flexi_streams
  module_function

  def octets_to_string(arr, opts = {})
    enc = opts[:"external-format"] && opts[:"external-format"].to_s || "utf-8"
    return arr.map(&:chr).join.force_encoding(enc)
  end
end
