using System;

namespace AABase.Logic
{
    public static class Utility
    {
        public static int FindPrecisionOfNumber(double num)
        {
            string numString = $"{ num}";
            int decIndex = numString.IndexOf(".");
            if (decIndex == -1) return 0;
            return numString.Substring(decIndex + 1).TrimEnd('0').Length;
        }
        public static bool EqualsWithEpsilon(double d1, double d2, double epsilon)
        {
            return (Math.Abs(d1 - d2) <= epsilon);
        }
        public static bool EqualsWithPrecision(double d1, double d2, int precision)
        {
            return EqualsWithEpsilon(d1, d2, Math.Pow(10, -1*precision));
        }
        public static double GetNormalizedAngle(double rad)
        {
           double remainder = Math.IEEERemainder(rad, Math.PI * 2.0);

            if (remainder < 0) 
                return remainder + Math.PI * 2.0;
            else
                return remainder;
         }
        public static double GetNextCoterminalAngle(double angle)
        {
            return angle + Math.PI * 2.0;
        }
        // assume thisAngle and startAngle are within range [0,2*PI], and endAngle within range [0,4*PI]
        public static bool IsAngleExclusivelyBetwStartAndEndAngles(double thisAngle, double startAngle, double endAngle)
        {
            // ensure endAngle is greater than startAngle
            if (startAngle > endAngle) endAngle = GetNextCoterminalAngle(endAngle);
            double thisAngleCoterminal = GetNextCoterminalAngle(thisAngle);
            return ((thisAngle > startAngle) && (thisAngle < endAngle)
                    ||
                    (thisAngleCoterminal > startAngle) && (thisAngleCoterminal < endAngle));
        }
    }
}
