module Sb_ext
  module_function

  def exit(opts = {})
    Kernel.exit
  end

  def quit(opts = {})
    Kernel.exit
  end

  def _invoke_debugger_hook_
    @@invoke_debugger_hook
  end

  def _invoke_debugger_hook_=(f)
    @@invoke_debugger_hook = f
  end
end
