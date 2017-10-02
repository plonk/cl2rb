
module Exit_hooks
  module_function

  def add_exit_hook(fun)
    at_exit { fun.call }
    return fun
  end
end
