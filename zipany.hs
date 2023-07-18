data HeadTail a = HeadTail a [a]

grow h [] (HeadTail hs ts) = HeadTail (h:hs) ts
grow h t (HeadTail hs ts) = HeadTail (h:hs) (t:ts)

heads ((x:xs):xss)= grow x xs (heads xss)
heads ([]:xss) = heads xss
heads [] = HeadTail [] []

zipAny [] = []
zipAny x = hs : (zipAny ts)
    where HeadTail hs ts = heads x

zipAnyWith f x = map f (zipAny x)

