# .zshenv

# MacOs started using something called path_helper and it is invoked
# *after* .zshenv. According to this gist:
#
# https://gist.github.com/Linerre/f11ad4a6a934dcf01ee8415c9457e7b2
#
# The order on is:
#
# /etc/zshenv
# ~/.zshenv
# (for login shells)
#   /etc/zprofile     <--- this calls path_helper and undoes .zshenv
#   ~/.zprofile
# (interactive)
#   /etc/zshrc
#   ~/.zshrc
# (login mode)
#   /etc/zlogin
#   ~/.zlogin
#
# I wanted to be a good zsh citizen and use .zshenv, but Apple fucked
# that up. So, the contents of .zshenv are going to .zprofile.
#
# Sigh.
#
# As I really don't use zsh features, I may just roll back to bash. 

# ... no code here ...
