module Test where
doubleSmallNumber x = if x > 100
                    then x
                    else x * 2
doubleLargeNumber x = (if x <= 100 then x else x * 2)+0