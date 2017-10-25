using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public static class VectorExtensions 
	{
		#region Rotate Vector Methods

		public static Vector2 Rotate (this Vector2 _vec, float _angleDeg)
		{
			float _angleRadians	= Mathf.Deg2Rad * _angleDeg;
			float _cosine		= Mathf.Cos(_angleRadians);
			float _sine			= Mathf.Sin(_angleRadians);

			Vector2 _rotatedVec	= Vector2.zero;
			_rotatedVec.x		= (_vec.x * _cosine) - (_vec.y * _sine);
			_rotatedVec.y		= (_vec.x * _sine) + (_vec.y * _cosine);

			return _rotatedVec;
		}

		#endregion
	}
}