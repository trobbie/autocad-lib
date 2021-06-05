
using System;

namespace AABase.Logic
{
    internal class AaGeArc : AaGeCurve
    {
        internal AaGeArc(AaPoint3d center, double radius, double startAngle, double endAngle, AaPoint3d planeNormal)
            : base(center, radius, startAngle, endAngle, planeNormal)
        {
            IsArc = true;
            Center = center;
            Radius = radius;
            StartAngle = (planeNormal.Z < 0) ? endAngle : startAngle;
            EndAngle = (planeNormal.Z < 0) ? startAngle + 2 * Math.PI : endAngle;
            // since flipping angles if negative-Z normal, also adjust the new plane normal
            PlaneNormal = (planeNormal.Z >= 0) ? planeNormal : new AaPoint3d(-planeNormal.X, -planeNormal.Y, -planeNormal.Z);
            _pt1 = null;
            _pt2 = null;
        }

        public override bool IsEqualTo(IGeCurve curve, bool ignorePointOrder)
        {
            // TODO: move these first two checks to parent class
            if (curve is null) return false;
            if (!IsArc.Equals(curve.IsArc)) return false;
            
            return (Center.Equals(curve.Center)
                    && Radius.Equals(curve.Radius)
                    && (StartAngle.Equals(curve.StartAngle) && EndAngle.Equals(curve.EndAngle))
                    || (ignorePointOrder && StartAngle.Equals(curve.EndAngle) && EndAngle.Equals(curve.StartAngle)));
        }

    }
}