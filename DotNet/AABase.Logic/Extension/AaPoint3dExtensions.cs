using System.Collections.Generic;
using System.Linq;

namespace AABase.Logic
{
    public static class AaPoint3dExtensions
    {
        /// <summary>
        /// Find the furthest point in direction of x-axis.
        /// </summary>
        /// <param name="points">List of points to consider</param>
        /// <param name="findMinimum">If true, find left-most point; if false, find right-most point</param>
        /// <param name="useMinimumYOnSameX">If true, use bottom-most point when x values equal; if false, use top-most point.</param>
        /// <returns>Furthest point in direction of x-axis</returns>
        public static AaPoint3d GetFurthestXPoint(this IEnumerable<AaPoint3d> points, bool findMinimum, bool useMinimumYOnSameX)
        {
            if (points.Count() == 0) return null;

            AaPoint3d furthestXPoint = points.First();
            foreach (AaPoint3d pt in points.Skip(1))
            {
                if (pt.X.IsEqualTo(furthestXPoint.X))
                {
                    if (useMinimumYOnSameX)
                        if (pt.Y < furthestXPoint.Y) furthestXPoint = pt; // use bottom-most
                    else
                        if (pt.Y > furthestXPoint.Y) furthestXPoint = pt; // use top-most
                }
                else if (pt.X < furthestXPoint.X)
                {
                    if (findMinimum) furthestXPoint = pt;
                }
                else 
                {
                    if (!findMinimum) furthestXPoint = pt;
                }
            }
            return furthestXPoint;
        }

        
    }
}
