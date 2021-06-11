using System.Collections.Generic;
using System.Linq;

namespace AABase.Logic
{
    public static class GeCurveExtensions
    {
        /// <summary>
        /// Find curves, not in curvesToIgnore, where the start or end point is at supplied point
        /// </summary>
        /// <param name="thesePolylines">Curve enumerable being extended</param>
        /// <param name="point3D">Supplied point</param>
        /// <param name="polylinesToIgnore">Polylines to ignore whether connecting to supplied point</param>
        /// <returns></returns>
        public static IEnumerable<IGeCurve> FindConnectingCurves(this IEnumerable<IGeCurve> curves, AaPoint3d pt, IEnumerable<IGeCurve> curvesToIgnore)
        {
            return curves.Where(pl => (pl.StartPoint.Equals(pt) || pl.EndPoint.Equals(pt))
                                       && !curvesToIgnore.Contains(pl)).ToList();
        }

    }
}