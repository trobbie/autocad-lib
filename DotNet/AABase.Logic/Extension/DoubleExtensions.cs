
using System;

namespace AABase.Logic
{
    public static class DoubleExtensions
    {
        public static bool IsEqualTo(this double num1, double num2)
        {
            return (Math.Abs(Math.Round(num1, AABaseLogicGlobal.MaxPointPrecision)
                             - Math.Round(num2, AABaseLogicGlobal.MaxPointPrecision))
                   <= Math.Pow(10, -1 * AABaseLogicGlobal.MaxPointPrecision));
        }
    }
}
