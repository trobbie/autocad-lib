using Microsoft.VisualStudio.TestTools.UnitTesting;
using AABase.Logic;
using AABase.Logic.Logging;
using System.Collections.Generic;
using System.Linq;

namespace AABase.Tests
{
  [TestClass]
    public class AaGeCurveConversionTests
    {
        private static readonly List<FakeEntity> emptyList = new List<FakeEntity>();
        private static readonly List<FakeEntity> triangleLines1 = new List<FakeEntity>();
        private static readonly List<FakeEntity> rectangleClosedPolyline1 = new List<FakeEntity>();
        private static readonly List<FakeEntity> rectangleOpenPolyline1 = new List<FakeEntity>();
        
       [ClassInitialize]
        public static void InitializeFakeData(TestContext testContext) {
            // triangle (as individual lines)
            triangleLines1.Add(new FakeLineEntity(0, 0, 10, 0));
            triangleLines1.Add(new FakeLineEntity(10, 0, 10, 10));
            triangleLines1.Add(new FakeLineEntity(10, 10, 0, 0));

            rectangleClosedPolyline1.Add(new FakeRectangle(new AaPoint3d(5,5), 7, 8, false));
            rectangleOpenPolyline1.Add(new FakeRectangle(new AaPoint3d(5,5), 7, 8, true));
        }
        
        static void TestConvertEntitiesToSimpleCurves(IEnumerable<IEntity>listObjects, List<IGeCurve> expected)
        {
            // Act
            IEnumerable<IGeCurve> test = listObjects.ConvertEntitiesToSimpleCurves();
            // Assert
            if (!test.OrderBy(t => t).SequenceEqual<IGeCurve>(expected.OrderBy(e => e), AaGeCurve.EqualValuesComparer))
                Assert.Fail($"Failed args {listObjects}.\nReturned {AaGeCurve.EnumerableToString(test)}.\nExpected {AaGeCurve.EnumerableToString(expected)}.");
        }

        [TestMethod]
        public void ConvertToCurveList_EmptyList_ReturnEmptyList()
        { 
            List<IGeCurve> listLinesExpected = new List<IGeCurve>();

            TestConvertEntitiesToSimpleCurves(emptyList, listLinesExpected);
        }

        [TestMethod]
        public void ConvertToCurveList_MultipleLines_ReturnValid()
        { 
            List<IGeCurve> listLinesExpected = new List<IGeCurve>();
            listLinesExpected.Add(AaGeCurve.Create(new AaPoint3d(0,0), new AaPoint3d(10,0)));
            listLinesExpected.Add(AaGeCurve.Create(new AaPoint3d(10,0), new AaPoint3d(10,10)));
            listLinesExpected.Add(AaGeCurve.Create(new AaPoint3d(10,10), new AaPoint3d(0,0)));

            TestConvertEntitiesToSimpleCurves(triangleLines1, listLinesExpected);
        }

        [TestMethod]
        public void ConvertToCurveList_OneClosedPolyline_ReturnValid()
        {
            List<IGeCurve> listLinesExpected = new List<IGeCurve>();
            listLinesExpected.Add(AaGeCurve.Create(new AaPoint3d(5,5), new AaPoint3d(12,5)));
            listLinesExpected.Add(AaGeCurve.Create(new AaPoint3d(12,5), new AaPoint3d(12,13)));
            listLinesExpected.Add(AaGeCurve.Create(new AaPoint3d(12,13), new AaPoint3d(5,13)));
            listLinesExpected.Add(AaGeCurve.Create(new AaPoint3d(5,13), new AaPoint3d(5,5)));

            TestConvertEntitiesToSimpleCurves(rectangleClosedPolyline1, listLinesExpected);
        }

        [TestMethod]
        public void ConvertToCurveList_OneOpenPolyline_ReturnValid()
        {
            List<IGeCurve> listLinesExpected = new List<IGeCurve>();
            listLinesExpected.Add(AaGeCurve.Create(new AaPoint3d(5,5), new AaPoint3d(12,5)));
            listLinesExpected.Add(AaGeCurve.Create(new AaPoint3d(12,5), new AaPoint3d(12,13)));
            listLinesExpected.Add(AaGeCurve.Create(new AaPoint3d(12,13), new AaPoint3d(5,13)));
            listLinesExpected.Add(AaGeCurve.Create(new AaPoint3d(5,13), new AaPoint3d(5,5)));

            TestConvertEntitiesToSimpleCurves(rectangleOpenPolyline1, listLinesExpected);
        }

    }
    
}