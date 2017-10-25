using UnityEngine;
using System.Collections;
using System.Globalization;

namespace VoxelBusters.Utility
{
	public static class DateTimeExtensions 
	{
		#region Constants

		private const string kZuluFormat	= "yyyy-MM-dd HH:mm:ss zzz";

		#endregion

		#region Create Methods
		
		public static System.DateTime ToDateTimeUTC (this string _string, string _format = null)
		{
			if (_string == null)
				return default(System.DateTime);
			
			return System.DateTime.ParseExact(_string, _format, CultureInfo.InvariantCulture).ToUniversalTime();
		}
		
		public static System.DateTime ToDateTimeLocal (this string _string, string _format = null)
		{
			if (_string == null)
				return default(System.DateTime);
			
			return System.DateTime.ParseExact(_string, _format, CultureInfo.InvariantCulture).ToLocalTime();
		}
		
		#endregion

		#region Zulu Format Extensions
				
		public static System.DateTime ToZuluFormatDateTimeUTC (this string _string)
		{
			if (_string == null)
				return default(System.DateTime);
			
			return System.DateTime.ParseExact(_string, kZuluFormat, CultureInfo.InvariantCulture).ToUniversalTime();
		}
		
		public static System.DateTime ToZuluFormatDateTimeLocal (this string _string)
		{
			if (_string == null)
				return default(System.DateTime);

			return System.DateTime.ParseExact(_string, kZuluFormat, CultureInfo.InvariantCulture).ToLocalTime();
		}

		public static string ToStringUsingZuluFormat (this System.DateTime _dateTime)
		{
			string 	_zuluFormatStringWithColon 	= _dateTime.ToString(kZuluFormat);
			int 	_stringLength				= _zuluFormatStringWithColon.Length;
			string 	_zuluFormatString			= _zuluFormatStringWithColon.Remove(_stringLength - 3, 1);
		
			return _zuluFormatString;
		}

		#endregion

		#region Java Platform Specific Conversion Methods

		public static System.DateTime ToDateTimeFromJavaTime(this long _time)
		{
			System.TimeSpan _timeSpanned 	= System.TimeSpan.FromMilliseconds(_time);
			System.DateTime _startDate		= new System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc);
			System.DateTime _dateTime 		= _startDate.Add(_timeSpanned);
			
			return _dateTime;
		}
		
		public static long ToJavaTimeFromDateTime(this System.DateTime _dateTime)
		{
			System.DateTime _startDate 		= new System.DateTime(1970, 1, 1, 0, 0, 0, System.DateTimeKind.Utc);
			long 			_timeInMillis 	= (long)(_dateTime.ToUniversalTime().Subtract(_startDate)).TotalMilliseconds;			

			return _timeInMillis;
		}

		#endregion
	}
}