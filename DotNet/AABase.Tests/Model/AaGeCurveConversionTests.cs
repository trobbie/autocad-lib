using Microsoft.VisualStudio.TestTools.UnitTesting;
using AABase.Logic;
using System.Collections.Generic;
using System.Linq;

namespace AABase.Tests
{
  [TestClass]
    public class AaLineExtensionsTests
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
        
        static void TestConvertEntitiesToCurveList(IEnumerable<IEntity>listObjects, List<AaGeCurve> expected)
        {
            // Act
            IEnumerable<AaGeCurve> test = AaGeCurve.ConvertEntitiesToCurveList(listObjects);
            // Assert
            if (!test.OrderBy(t => t).SequenceEqual<AaGeCurve>(expected.OrderBy(e => e), AaGeCurve.EqualValuesComparer))
                Assert.Fail($"Failed args {listObjects}.\nReturned {AaGeCurve.EnumerableToString(test)}.\nExpected {AaGeCurve.EnumerableToString(expected)}.");
        }

        [TestMethod]
        public void ConvertToCurveList_EmptyList_ReturnEmptyList()
        { 
            List<AaGeCurve> listLinesExpected = new List<AaGeCurve>();

            TestConvertEntitiesToCurveList(emptyList, listLinesExpected);
        }

        [TestMethod]
        public void ConvertToCurveList_MultipleLines_ReturnValid()
        { 
            List<AaGeCurve> listLinesExpected = new List<AaGeCurve>();
            listLinesExpected.Add(new AaGeCurve(new AaPoint3d(0,0), new AaPoint3d(10,0)));
            listLinesExpected.Add(new AaGeCurve(new AaPoint3d(10,0), new AaPoint3d(10,10)));
            listLinesExpected.Add(new AaGeCurve(new AaPoint3d(10,10), new AaPoint3d(0,0)));

            TestConvertEntitiesToCurveList(triangleLines1, listLinesExpected);
        }

        [TestMethod]
        public void ConvertToCurveList_OneClosedPolyline_ReturnValid()
        {
            List<AaGeCurve> listLinesExpected = new List<AaGeCurve>();
            listLinesExpected.Add(new AaGeCurve(new AaPoint3d(5,5), new AaPoint3d(12,5)));
            listLinesExpected.Add(new AaGeCurve(new AaPoint3d(12,5), new AaPoint3d(12,13)));
            listLinesExpected.Add(new AaGeCurve(new AaPoint3d(12,13), new AaPoint3d(5,13)));
            listLinesExpected.Add(new AaGeCurve(new AaPoint3d(5,13), new AaPoint3d(5,5)));

            TestConvertEntitiesToCurveList(rectangleClosedPolyline1, listLinesExpected);
        }

        [TestMethod]
        public void ConvertToCurveList_OneOpenPolyline_ReturnValid()
        {
            List<AaGeCurve> listLinesExpected = new List<AaGeCurve>();
            listLinesExpected.Add(new AaGeCurve(new AaPoint3d(5,5), new AaPoint3d(12,5)));
            listLinesExpected.Add(new AaGeCurve(new AaPoint3d(12,5), new AaPoint3d(12,13)));
            listLinesExpected.Add(new AaGeCurve(new AaPoint3d(12,13), new AaPoint3d(5,13)));
            listLinesExpected.Add(new AaGeCurve(new AaPoint3d(5,13), new AaPoint3d(5,5)));

            TestConvertEntitiesToCurveList(rectangleOpenPolyline1, listLinesExpected);
        }

    }
    
}