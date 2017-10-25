using UnityEngine;
using System.Collections;
using System;
using System.Collections.Generic;

namespace VoxelBusters.RuntimeSerialization.Benchmark
{
	[System.Serializable, RuntimeSerializable]
	public class BenchmarkSampleBase
	{
		#region Fields

		[Multiline]
		public 			string					baseClassStringField;
		public 			int						baseClassIntField;
		public 			float					baseClassFloatField;

		#endregion

		#region Constructors

		public BenchmarkSampleBase ()
		{}

		public BenchmarkSampleBase (bool _random)
		{
			// Initialise
			baseClassStringField	= System.IO.Path.GetRandomFileName();
			baseClassIntField		= UnityEngine.Random.Range(1, 1000);
			baseClassFloatField		= UnityEngine.Random.Range(1f, 50f);
		}

		#endregion
	}

	[System.Serializable, RuntimeSerializable(true, true)]
	public class BenchmarkSample : BenchmarkSampleBase
	{
		#region Fields

		[NonRuntimeSerializedField]
		public 			int						unserializedIntField;
		public 			ushort					ushortField;
		public 			List<int>				intList;
		public 			DateTime				dateTimeField;

#pragma warning disable
		[NonRuntimeSerializedField]
		private 		string					m_unserializedString;
		private 		string[]				m_privateStringArray;
#pragma warning restore

		#endregion

		#region Constructors
		
		public BenchmarkSample ()
		{}
		
		public BenchmarkSample (bool _random) : base (_random)
		{
			// Initialise
			unserializedIntField	= UnityEngine.Random.Range(1, 1000);
			ushortField				= (ushort)UnityEngine.Random.Range(1, 1000);
			intList					= new List<int> (new int [] {
				UnityEngine.Random.Range(1, 1000),
				UnityEngine.Random.Range(1, 1000),
				UnityEngine.Random.Range(1, 1000),
			});
			DateTime 	_startDate	= new DateTime(1947, 1, 1);
			int _range 				= (DateTime.Today - _startDate).Days;           
			dateTimeField			= _startDate.AddDays(UnityEngine.Random.Range(0, _range));
			m_unserializedString	= System.IO.Path.GetRandomFileName();
			m_privateStringArray	= new string[] {
				System.IO.Path.GetRandomFileName(),
				System.IO.Path.GetRandomFileName(),
				System.IO.Path.GetRandomFileName()
			};
		}

		#endregion
	}
}