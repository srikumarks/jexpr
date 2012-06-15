for { i: from 1 to: 10
    , j: from 1 to: 10 when: (fn j to: math (j * 2) > i and (i < 5))
    }
body: 
    display (list i j)
