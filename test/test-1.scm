(new-topic "default")
; -> 1
(new-subject "default" "foo")
; -> 1
(new-subject "default" "bar")
; -> 2
(new-subject "default" "meow")
; -> 3
(new-object "default" "01")
; -> 1
(new-object "default" "02")
; -> 2
(new-object "default" "03")
; -> 3
(new-object "default" "04")
; -> 4
(new-object "default" "05")
; -> 5
(set-subject-pref "default" "bar" '(("01" . "4")))
; -> (1)
(set-subject-pref "default" "foo" '(("01" . "2") ("02" . "4") ("03" . "1") ("04" . "3") ("05" . "5")))
; -> (2 3 4 5 6)
(set-subject-pref "default" "meow" '())
; -> ()
(set-config "json-prefix" "test-")
(set-config "server-port" 9090)
(generate-json)
(exit)
