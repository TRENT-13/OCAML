type quadtree_node = NoPoint 
      | Point of int * int
      |Qnode of quadtree_node 
                * quadtree_node
                * quadtree_node
                * quadtree_node

                

type quadtree = {width:int; height: int; root: quadtree_node};;

let insert (px, py) qtree  =
  let rec impl (x1,y1,x2,y2) (px,py) = 
    let xmid = (x1+x2) /2 in
    let ymid = (y1+y2) /2 in
    function NoPoint -> Point(px, py)
      | Qnode (nn,np,pn,pp) ->
        (match px < xmid, py < ymid with
        | true , true -> Qnode(impl (x1,y1, xmid,ymid) (px,py) nn, np, pn,pp)
        | true, false -> Qnode(nn, impl(x1,ymid, xmid,y2) (px,py) np, pn, pp)
        | false, true -> Qnode(nn, np, impl (xmid, y1, x2,ymid) (px, py) pn, pp)
        | false, false -> Qnode (nn, np, pn, impl(xmid, ymid, y1,y2) (px, py) pp))
      |Point (px',py') -> if (px,py) = (px',py')then Point (px, py)
      else impl (x1,y1,x2,y2) (px',py') (impl (x1,y1,x2,y2) (px, py) (Qnode (NoPoint, NoPoint, NoPoint, NoPoint)))
    in { qtree with root = impl (0,0, qtree.width, qtree.height) (px,py) qtree.root};;


    let a66_t = {width=16; height=16; root=NoPoint};;

    let insert_points = List.fold_left (fun t p -> insert p t) a66_t;;
  (insert_points[5,5]).root
