-- Move to different direction
tallRightCorner (x,y) = (x+1,y-2)
tallRightBCorner (x,y) = (x+1, y+2)

tallLeftCorner (x,y) = (x-1, y-2)
tallLeftBCorner (x,y) = (x-1, y+2)

shortRightCorner (x,y) = (x+2,y-1)
shortRightBCorner (x,y) = (x+2,y+1)

shortLeftCorner (x,y) = (x-2, y-1)
shortLeftBCorner (x,y) = (x-2, y+1)

canMove p points = p `elem` points

createMap w h = [(x,y) | x <- [1..w], y <-[1..h]]
moveToPoint p routes points
            | canMove (tallRightCorner p) points = moveToPoint (tallRightCorner p) r l
            | canMove (tallRightBCorner p) points = moveToPoint (tallRightBCorner p) r l
            | canMove (tallLeftCorner p) points = moveToPoint (tallLeftCorner p) r l
            | canMove (tallLeftBCorner p) points = moveToPoint (tallLeftBCorner p) r l
            | canMove (shortRightCorner p) points = moveToPoint (shortRightCorner p) r l
            | canMove (shortRightBCorner p) points = moveToPoint (shortRightBCorner p) r l
            | canMove (shortLeftCorner p) points = moveToPoint (shortLeftCorner p) r l
            | canMove (shortLeftBCorner p) points = moveToPoint (shortLeftBCorner p) r l
            | otherwise = routes ++ [p]
		    where r = routes ++ [p]
		          l  = [n | n<-points, n /=p]	  


notOverBound (a,b) w h = let wRange = [1..w]
                             hRange = [1..h]
                         in (a `elem` wRange) && (b `elem` hRange)

startToMove p routes points w h = 
    if (length points ) > 0
        then do
            let r = routes ++ [p]
            let l = [n | n<-points, n /=p, p `elem` points]	  
            startToMove (tallRightBCorner p) r l w h
            startToMove (tallLeftCorner p) r l w h
            startToMove (tallLeftBCorner p) r l w h
            startToMove (shortRightCorner p) r l w h
            startToMove (shortRightBCorner p) r l w h
            startToMove (shortLeftCorner p) r l w h
            startToMove (shortLeftBCorner p) r l w h
        else
            if ((length routes) == w*h ) && (notOverBound (last routes) w h) && (not ((last routes) `elem` (init routes)))
                then putStrLn $ "Move: "++ show routes ++ "\n\n\n"
                else return ()

-- Solve the house game -- Input: inital pointer eg. (1, 1), weight, height, weight, height
-- Output: IO action that output the movement of the house
-- Sample Input: houseSolver (1,1) 5 5 5 5
houseSolver p w h = let initialMap = createMap w h
                    in startToMove p [] initialMap
    
houseGame x y w h = let initialMap = createMap w h
                        initialPoint = (x,y)
                    in moveToPoint initialPoint [] initialMap

