module Stack (Stack ,createEmptyStack , isEmpty ,push , pop , top) where 
    data Stack a = St [a]

    createEmptyStack :: Stack a
    createEmptyStack = St []
    
    isEmpty :: Stack a -> Bool
    isEmpty (St []) = True
    isEmpty _ = False
    
    push :: a -> Stack a -> Stack a
    push x (St xs) = St (x:xs)
    
    pop :: Stack a -> Stack a
    pop (St []) = error "Empty␣stack"
    pop (St (x:xs)) = St xs
    
    top :: Stack a -> a
    top (St []) = error "Empty␣stack"
    top (St (x:xs)) = x