{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import GHC.Exts.Heap (IndexTable(it_back_edge), GenClosure (n_args))
import Data.List
import Data.Ord

type Point = (Float,Float) -- 3D point locations
type Line = (Point, Point, Float) -- Two points for a line and the width
type Triangle = (Point, Point, Point) --To save STL files

sortClockwise :: [Point] -> [Point]
sortClockwise ps = sortBy (flip $ comparing (angle c)) ps
  where
    c = (avg (map fst ps), avg (map snd ps))
    avg xs = sum xs / fromIntegral (length xs)
    angle (cx, cy) (x, y) = atan2 (y - cy) (x - cx)


line_list :: [Line]
line_list = [((-5,-0.25),(5,0.25),1)]

create_triangles_from_rectangle :: [Point] -> [Triangle]
create_triangles_from_rectangle corners = [tri1, tri2, tri3, tri4]
    where
        ([x1,x2,x3,x4], [y1,y2,y3,y4]) = unzip(sortClockwise corners)
        middle_point = ((x1+x2+x3+x4) /4, (y1+y2+y3+y4)/4)
        tri1 = ((x1,y1), middle_point, (x2,y2))
        tri2 = ((x2,y2), middle_point, (x3,y3))
        tri3 = ((x3,y3), middle_point, (x4,y4))
        tri4 = ((x4,y4), middle_point, (x1,y1))


get_rectangle_corners :: Line -> [(Float,Float)]
get_rectangle_corners ((x1,y1), (x2,y2), width) = [(x1 + hwx, y1 + hwy), (x1 - hwx, y1 - hwy), (x2 - hwx, y2 - hwy), (x2 + hwx, y2 + hwy)]
    where
        length = sqrt((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1))
        nx = -(y2 - y1) / length
        ny = (x2 - x1) / length
        hwx = nx * width / 2
        hwy = ny * width / 2 

linelist_to_rects :: [Line] -> [Triangle]
linelist_to_rects line_list = concat [create_triangles_from_rectangle (get_rectangle_corners line) | line <- line_list]

createTriangleDef :: Triangle -> String
createTriangleDef ((x1,y1),(x2,y2),(x3,y3)) = "  facet\n" ++
                                                       "    outer loop\n" ++
                                                       "      vertex " ++ (show x1) ++ " " ++ (show y1) ++ " 0 \n" ++
                                                       "      vertex " ++ (show x2) ++ " " ++ (show y2) ++ " 0 \n" ++
                                                       "      vertex " ++ (show x3) ++ " " ++ (show y3) ++ " 0 \n" ++
                                                       "    endloop\n" ++
                                                       "  endfacet\n"                            

createObjectModelString :: [Triangle] -> String 
createObjectModelString n = "solid Object01\n" ++ concat [createTriangleDef y | y<-n] ++ "endsolid Object01"



writeObjModel :: [Triangle] -> String -> IO ()
writeObjModel x filename = do writeFile filename (createObjectModelString x)

hilbertcurve :: Integer -> [Line]
hilbertcurve orderI =
  let
    order = fromIntegral orderI :: Int
    n = 2 ^ order
    total = n * n

    hilbert :: Int -> Int -> (Int, Int)
    hilbert ord i = loop 0 (0, 0)
      where
        loop j (x, y)
          | j >= ord = (x, y)
          | otherwise =
              let len = 2 ^ j
                  index = (i `div` (4 ^ j)) `mod` 4
                  (x', y') = case index of
                    0 -> (y, x)
                    1 -> (x, y + len)
                    2 -> (x + len, y + len)
                    3 -> let x'' = len - 1 - x
                             y'' = len - 1 - y
                         in (y'' + len, x'')
              in loop (j + 1) (x', y')

    scale = 10 / fromIntegral n
    thickness = scale * 0.4
    points = [ hilbert order i | i <- [0 .. total - 1] ]
    scaledPoints = map (\(x, y) -> (fromIntegral x * scale, fromIntegral y * scale)) points

    toLines (p1:p2:rest) = (p1, p2, thickness) : toLines (p2:rest)
    toLines _ = []
  in
    toLines scaledPoints

main = do writeObjModel (linelist_to_rects (hilbertcurve 5)) "hilbert.stl" 


    

    


    
