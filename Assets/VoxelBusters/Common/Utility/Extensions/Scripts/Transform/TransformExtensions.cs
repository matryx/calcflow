using UnityEngine;
using System.Collections;

namespace VoxelBusters.Utility
{
	public static class TransformExtensions
	{
		public static string GetPath (this Transform _transform)
		{
			if (_transform == null)
				return null;

			Transform _parentTransform	= _transform.parent;

			if (_parentTransform == null)
				return "/" + _transform.name;

			return _parentTransform.GetPath() + "/" + _transform.name;
		}
	}
}