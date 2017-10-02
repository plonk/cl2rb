module Ql
  module_function

  def quickload(library)
    require_relative library.to_s.gsub('-', '_')
    STDERR.puts("Quickload #{library}")
    [library]
  end
end
