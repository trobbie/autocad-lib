using System;
using System.Linq;
using System.Collections.Generic;

namespace AABase.Logic
{
    public static class AaGeCurveExtensions
    {
        private static IEnumerable<string> _supportedDxfNamesForGetCurveList = new List<string> { "LINE", "LWPOLYLINE", "ARC", "CIRCLE" };

        public static bool IsDxfNameSupportedForGetCurveList(string dxfName) 
        {
            return _supportedDxfNamesForGetCurveList.Contains(dxfName);
        }
        public static string ToStringDebug(this IEnumerable<AaGeCurve> listCurves)
        {
            return listCurves.Select(i => i.ToString())
                            .Aggregate("",(i,j) => i + "," + j);
        }

        public static List<AaGeCurve> GetCurveList(this IEnumerable<IEntity> listCurves)
        {
            List<AaGeCurve> result = new List<AaGeCurve>();

            // for each supported entity, add to the list of curves
            foreach (IEntity entity in listCurves)
            {
                switch (entity.GetDxfName())
                {
                    case "LINE":
                        ICurve line = (ICurve)entity;
                        result.Add(new AaGeCurve(line.StartPoint, line.EndPoint));
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
                        ICircle circle = (ICircle)entity;
                        result.Add(new AaGeCurve(circle.Center, circle.Radius, 0, 2*Math.PI, circle.PlaneNormal));
                        break;
                    case "ARC":
                        IArc arc = (IArc)entity;
                        result.Add(new AaGeCurve(arc.Center, arc.Radius, arc.StartAngle, arc.EndAngle, arc.PlaneNormal));
                        break;
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
