#lang knight
; = fizzbuzz BLOCK
    ; = n 0
    : WHILE < n max
        ; = n + n 1
        : OUTPUT
            : IF ! (% n 15) "FizzBuzz"
            : IF ! (% n 5)  "Fizz"
            : IF ! (% n 3)  "Buzz"
                            n

; = max 100
: CALL fizzbuzz
