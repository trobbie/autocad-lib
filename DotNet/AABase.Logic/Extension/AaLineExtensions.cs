using System;
using System.Linq;
using System.Collections.Generic;

namespace AABase.Logic
{
    public static class AaLineExtensions
    {
        public static string ToStringDebug(this IEnumerable<AaLine> listLines)
        {
            return listLines.Select(i => i.ToString())
                            .Aggregate("",(i,j) => i + "," + j);
        }
        public static List<AaLine> GetLineList(this IEnumerable<IEntity> listLines)
        {
            List<AaLine> result = new List<AaLine>();

            // for each supported entity, add to the list of curves
            foreach (IEntity entity in listLines)
            {
                switch (entity.GetDxfName())
                {
                    case "LINE":
                        ICurve l = (ICurve)entity;
                        result.Add(new AaLine(l.StartPoint, l.EndPoint));
                        break;
                    case "LWPOLYLINE": // light-weight polyline
                        IPolyline pl = (IPolyline)entity;
                        int numVertices = pl.NumberOfVertices;
                        int iNext;
                        for (int i = 0; i < numVertices; i++)
                        {
                            iNext = i + 1;
                            if (iNext == numVertices)
                            {
                                if (pl.Closed)
                                    iNext = 0;
                                else
                                    continue; // done with polyline
                            }
                            if (pl.IsArcSegment(i))
                            {
                                throw new NotImplementedException();
                            }
                            else
                            {
                                result.Add(new AaLine(pl.GetPoint3dAt(i), pl.GetPoint3dAt(iNext)));
                            }
                        }
                        break;
                    case "CIRCLE": // add "quadrant" arcs of circle
                        throw new NotImplementedException();
                    case "ARC": // add start and end points, and any "full circle" extents also on its arc extents
                        throw new NotImplementedException();
                    case "BLOCK":  // break first
                        throw new NotImplementedException();
                    default:
                        throw new NotImplementedException();
                }
            }

            return result;
        }
    }
}
