(new-subject "foo")
; -> 1
(new-subject "bar")
; -> 2
(new-subject "meow")
; -> 3
(new-object "01")
; -> 1
(new-object "02")
; -> 2
(new-object "03")
; -> 3
(new-object "04")
; -> 4
(new-object "05")
; -> 5
(set-subject-pref "bar" '(("01" . "4")))
; -> (1)
(set-subject-pref "foo" '(("01" . "2") ("02" . "4") ("03" . "1") ("04" . "3") ("05" . "5")))
; -> (2 3 4 5 6)
(set-subject-pref "meow" '())
; -> ()
(set-config "json-prefix" "test-")
(set-config "server-port" 9090)
(generate-json)
(exit)
