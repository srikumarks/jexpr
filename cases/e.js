macro hello
    lambda: expr
    body: ($_ (display (list "In macro!" (_$$ meow ($_ (list (_$ expr.hello)))))))
    where: {meow: $ "bowow"}
hello "macro" "world!"
