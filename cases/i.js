let {ruler: lambda arg body:
                if keywords.double_rule
                    then: (display "===================")
                    else: (display "-------------------")
                display arg
     , another: "yeow"}
in: 
    ruler (list "An important message!" another) 
        double_rule: true
