ExUnit.configure(exclude: [:skip | ExUnit.configuration()[:exclude]])
ExUnit.start()
Application.ensure_all_started(:bypass)

File.rm_rf!(HexTest.Case.tmp_path())
File.mkdir_p!(HexTest.Case.tmp_path())
HexTest.Case.init_reset_state()
