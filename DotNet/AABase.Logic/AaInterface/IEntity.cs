
namespace AABase.Logic
{
    public interface IEntity : IDBObject
    {
        object GetAcEntity();

        IExtents3d GeometricExtents { get; }

        double GetLength();

        /// <summary>
        /// Set an entity's layer
        /// </summary>
        /// <param name="layerName">Name of layer.</param>
        void SetLayer(string layerName);
    }
}
