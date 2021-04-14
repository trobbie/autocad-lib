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

        // TODO: iterate through each curve and return list of lines that are overlapping.
        // Two segments overlap if either vertex of a segment lies directly on the other segment,
        // or both vertices are coincident with the two vertices of the other segment.
    }
}
