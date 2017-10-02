module Sb_ext
  module_function

  def exit(opts = {})
    Kernel.exit
  end

  def quit(opts = {})
    Kernel.exit
  end
end
