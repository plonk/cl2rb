# require 'curses'
# なんで失敗するん。Object のメソッド上書きしすぎたか。

module Cl_charms_low_level
  module_function

  def initscr()
    Curses.init_screen()
  end

  def _stdscr_
    Curses.stdscr
  end

  def clearok(win, flag)
  end

  def scrollok(win, flag)
    win.scrollok(true)
  end

  def keypad(win, flag)
    win.keypad = (flag != 0)
  end

  def raw
    Curses.raw
  end

  def cbreak
    Curses.cbreak
  end

  def endwin
    Curses.close_screen
  end

  def clear
    Curses.clear
  end

  def addstr(str)
    Curses.addstr(str)
  end

  def getcurx(win)
    # Ruby curses ではカーソル位置はわからないようだ。
    1
  end

  def getch
    c = Curses.getch
    case c
    when String
      return c.ord
    else
      return c
    end
  end

  KEY_UP    = Curses::Key::UP
  KEY_LEFT  = Curses::Key::LEFT
  KEY_DOWN  = Curses::Key::DOWN
  KEY_RIGHT = Curses::Key::RIGHT

end

Charms_ll = Cl_charms_low_level
