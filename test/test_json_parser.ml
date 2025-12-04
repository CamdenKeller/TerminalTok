open OUnit2
open Terminal_tok.Json_parser
open Terminal_tok.Types

let test_parse_valid_json _ =
  let json_content =
    {|
    {
      "camels": [
        {
          "name": "Camel 1",
          "genre": "comedy",
          "ascii": "  O  \n /|\\ "
        },
        {
          "name": "Camel 2",
          "genre": "action",
          "ascii": "  X  \n /|\\ "
        }
      ]
    }
    |}
  in
  let filename = "dummy.json" in
  let oc = open_out filename in
  output_string oc json_content;
  close_out oc;

  let result = parse_camels filename in
  Sys.remove filename;

  assert_equal 2 (List.length result);
  let v1 = List.nth result 0 in
  assert_equal "Camel 1" v1.title;
  assert_equal "comedy" v1.genre;
  assert_equal "  O  \n /|\\ " v1.ascii

let test_parse_videos _ =
  let json_content =
    {|
    {
      "videos": [
        {
          "name": "Video 1",
          "genre": "Genre 1",
          "file": "path/to/video1.mp4"
        },
        {
          "name": "Video 2",
          "genre": "Genre 2",
          "file": "path/to/video2.mp4"
        }
      ]
    }
    |}
  in
  let filename = "dummy_videos.json" in
  let oc = open_out filename in
  output_string oc json_content;
  close_out oc;

  let result = parse_videos filename in
  Sys.remove filename;

  assert_equal 2 (List.length result);
  let (t1, g1, f1) = List.nth result 0 in
  assert_equal "Video 1" t1;
  assert_equal "Genre 1" g1;
  assert_equal "path/to/video1.mp4" f1

let test_parse_malformed_json _ =
  let json_content = "{ invalid_json: " in
  let filename = "malformed.json" in
  let oc = open_out filename in
  output_string oc json_content;
  close_out oc;

  assert_raises (Yojson.Json_error "Line 1, bytes 15-16:\nUnexpected end of input") (fun () -> parse_camels filename);
  Sys.remove filename

let test_parse_empty_file _ =
  let filename = "empty.json" in
  let oc = open_out filename in
  close_out oc;

  assert_raises (Yojson.Json_error "Blank input data") (fun () -> parse_camels filename);
  Sys.remove filename

let tests =
  "json parser tests"
  >::: [
         "parse valid json" >:: test_parse_valid_json;
         "parse videos json" >:: test_parse_videos;
         "parse malformed json" >:: test_parse_malformed_json;
         "parse empty file" >:: test_parse_empty_file;
       ]

let _ = run_test_tt_main tests
