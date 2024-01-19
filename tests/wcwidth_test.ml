let test_wcswidth str expected () =
  let actual = Wcwidth.wcswidth str in
  Alcotest.(check int) "same width" expected actual

let () =
  let open Alcotest in
  run "package:wcwidth" [
    "function:wcswidth", [
      test_case "ASCII" `Quick (test_wcswidth "Hello, Tom!" 11);
      test_case "Empty string" `Quick (test_wcswidth "" 0);
      test_case "Newline" `Quick (test_wcswidth "\n" (-1));
    ];
  ]
