
using System;

namespace AABase.Logic
{
    public static class DoubleExtensions
    {
        public static bool IsEqualTo(this double num1, double num2)
        {
            return Utility.EqualsWithPrecision(num1, num2, AABaseLogicGlobal.MaxPointPrecision);
            
        }
    }
}
