using System.Collections.Generic;
using System.Linq;
using AABase.Logic;

namespace AABase.Logic.Service
{
    public class CurvesService
    {
        private readonly IEnumerable<ICurve> _curves;

        public CurvesService(IEnumerable<ICurve> curves)
        {
            this._curves = curves;
        }

        public double CalculateTotalLength()
        {
            return _curves
                    .Select(curve => curve.GetLength())
                    .Sum();
        }
    }
}
