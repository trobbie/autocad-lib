using AABase.Logic;

namespace AABase.Tests
{
    internal class FakeEntity : IEntity
    {
        public IExtents3d GeometricExtents => throw new System.NotImplementedException();

        public object GetAcEntity()
        {
            throw new System.NotImplementedException();
        }

        public virtual string GetDxfName()
        {
            throw new System.NotImplementedException();
        }

        public object GetID()
        {
            throw new System.NotImplementedException();
        }

        public double GetLength()
        {
            throw new System.NotImplementedException();
        }

        public void SetLayer(string layerName)
        {
            throw new System.NotImplementedException();
        }
    }
}