using AABase.Logic.Model;

namespace AABase.Logic
{
    public interface ICurve : IEntity
    {
        AaPoint3d StartPoint { get; }
        AaPoint3d EndPoint { get; }
    }
}
