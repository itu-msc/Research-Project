# 0.3.0 - 2025-12-15

- Fixed bug in heap related to garbage colletcion.

# 0.2.0 - 2025-12-05

- Changed signal combinators to have `*L` suffix for later signals, rather than `*D`.
- Swapped order of arguments for `|>>` (applicative operator) to better fit a left-to-right reading direction, and replaced to previous order (function first) with `<<|`.
- Overall (and totally unbiased), a much better version. 

# 0.1.0 - 2025-11-27

- Initial release of the `Rizzo` FRP library.
- Core modules: generic `signal`, `channel`, and `types`, including event loop and heap internals.
- Input handling via `console_input` and `port_input`.
- Output handling via `console_output` and `port_send_outputD`.
