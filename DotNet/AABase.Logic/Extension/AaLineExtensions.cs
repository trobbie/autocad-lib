using System;
using System.Linq;
using System.Collections.Generic;

namespace AABase.Logic
{
    public static class AaLineExtensions
    {
        private static IEnumerable<string> _supportedDxfNamesForGetLineList = new List<string> { "LINE", "LWPOLYLINE" };

        public static bool IsDxfNameSupportedForGetLineList(string dxfName) 
        {
            return _supportedDxfNamesForGetLineList.Contains(dxfName);
        }
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
                                // TODO: support arcs; add a CircularArc3d base type
                                throw new NotImplementedException();
                            }
                            else
                            {
                                result.Add(new AaLine(pl.GetPoint3dAt(i), pl.GetPoint3dAt(iNext)));
                            }
                        }
                        break;
                    case "CIRCLE":
                        // TODO: support arcs; add a CircularArc3d base type
                        throw new NotImplementedException();
                    case "ARC":
                        // TODO: support arcs; add a CircularArc3d base type
                        throw new NotImplementedException();
                    case "BLOCK":
                        // TODO: support blocks
                        throw new NotImplementedException();
                    default:
                        throw new NotImplementedException();
                }
            }

            return result;
        }

        public static List<AaGeCurve> GetCurveList(this IEnumerable<IEntity> listLines)
        {
            List<AaGeCurve> result = new List<AaGeCurve>();

            // for each supported entity, add to the list of curves
            foreach (IEntity entity in listLines)
            {
                switch (entity.GetDxfName())
                {
                    case "LINE":
                        ICurve l = (ICurve)entity;
                        result.Add(new AaGeCurve(l.StartPoint, l.EndPoint));
                        break;
                    case "LWPOLYLINE": // light-weight polyline
                        IPolyline pl = (IPolyline)entity;
                        int numVertices = pl.NumberOfVertices;
                        for (int i = 0; i < numVertices; i++)
                        {
                            // if on last vertex and closed, we're done with adding curves
                            if (((i+1) == numVertices) && (!pl.Closed)) continue;
                            result.Add(pl.GetGeCurveAt(i));
                        }
                        break;
                    case "CIRCLE":
                        // TODO: support arcs; add a CircularArc3d base type
                        throw new NotImplementedException();
                    case "ARC":
                        // TODO: support arcs; add a CircularArc3d base type
                        throw new NotImplementedException();
                    case "BLOCK":
                        // TODO: support blocks
                        throw new NotImplementedException();
                    default:
                        throw new NotImplementedException();
                }
            }

            return result;
        }
    }
}
