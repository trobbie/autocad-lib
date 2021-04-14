using System;

namespace Autodesk.AutoCAD.DatabaseServices
{
    /// <summary>
    /// 
    /// </summary>
    public static class CurveExtensions
    {
        //
        /// <summary>
        /// Calculate the length of a curve.
        /// </summary>
        /// <author>Modified from original authors: Tony Tanzillo</author>
        /// <param name="curve">The curve.</param>
        /// <returns></returns>
        /// <exception cref="ArgumentNullException">Curve was Null</exception>
        public static double GetLength(this Curve curve)
        {
            if (curve == null)
            {
                throw new ArgumentNullException("Curve was Null");
            }

            if (curve is Xline || curve is Ray)
            {
                return Double.PositiveInfinity;
            }

            return
                Math.Abs(curve.GetDistanceAtParameter(curve.EndParam) - curve.GetDistanceAtParameter(curve.StartParam));
        }
    }
}
