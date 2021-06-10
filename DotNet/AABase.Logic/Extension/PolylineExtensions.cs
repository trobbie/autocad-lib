using System.Collections.Generic;
using System.Linq;

namespace AABase.Logic
{
    public static class PolylineExtensions
    {
        /// <summary>
        /// Find polylines within these polylines, but not in polylinesToIgnore, where the start or end point is at supplied point
        /// </summary>
        /// <param name="thesePolylines">Polyline enumerable being extended</param>
        /// <param name="point3D">Supplied point</param>
        /// <param name="polylinesToIgnore">Polylines to ignore whether connecting to supplied point</param>
        /// <returns></returns>
        public static IEnumerable<IPolyline> FindConnectingPolylines(this IEnumerable<IPolyline> thesePolylines, AaPoint3d pt, IEnumerable<IPolyline> polylinesToIgnore)
        {
            return thesePolylines.Where(pl => (pl.StartPoint.Equals(pt) || pl.EndPoint.Equals(pt))
                                       && !polylinesToIgnore.Contains(pl)).ToList();
        }

    }
}