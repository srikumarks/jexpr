do
    display "hello world using fn"
    let {f: fn x to: x.length} in:
       display (f "meow")
