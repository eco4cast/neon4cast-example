library(decor)
cpp11::cpp_source(code = '
#include <csignal>
#include <cpp11.hpp>

[[cpp11::register]] void ignore_sigpipes() {
  signal(SIGPIPE, SIG_IGN);
}
')
ignore_sigpipes()
