using AABase.Logic.Model;

namespace AABase.Logic.AaInterface
{
    public interface IArc : ICurve
    {
        AaPoint3d Center { get; }
        double Radius { get; }
    }
}
