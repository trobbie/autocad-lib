using System;
using System.Linq;
using System.Collections.Generic;

namespace AABase.Logic
{
    public static class IEntityExtensions
    {
        public static List<AaGeCurve> ConvertEntitiesToCurveList(this IEnumerable<IEntity> entities, ILogWriter logger)
        {
            List<AaGeCurve> result = new List<AaGeCurve>();

            // filter out supported entities (perhaps later support)
            IEnumerable<IEntity> supportedEntities = entities.Where(ent => {
                if (AaGeCurve.SupportsCurveConversions(ent.GetDxfName()))
                {
                    return true;
                } 
                else 
                {
                    logger.WriteLine(LogLevel.Warning, $"Found object type {ent.GetDxfName()}.  Completely ignoring this unsupported type.");
                    return false;
                }
            });
            
            // for each supported entity, add to the list of curves
            foreach (IEntity entity in supportedEntities)
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