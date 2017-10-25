using UnityEngine;
using System.Collections;
using System;

#if NETFX_CORE
using System.Collections.Generic;
using System.Reflection;
#endif

namespace VoxelBusters.Utility
{
	public static class TypeExtensions  
	{

#if !NETFX_CORE
		#region Properties

		private static		Type[]			typeCodesToType;

		#endregion

		#region Constructors
		
		static TypeExtensions ()
		{
			typeCodesToType		= new Type[19];
			typeCodesToType[1]	= typeof(System.Object);
			typeCodesToType[2]	= typeof(DBNull);
			typeCodesToType[3]	= typeof(bool);
			typeCodesToType[4]	= typeof(char);
			typeCodesToType[5]	= typeof(sbyte);
			typeCodesToType[6]	= typeof(byte);
			typeCodesToType[7]	= typeof(Int16);
			typeCodesToType[8]	= typeof(UInt16);
			typeCodesToType[9]	= typeof(Int32);
			typeCodesToType[10]	= typeof(UInt32);
			typeCodesToType[11]	= typeof(Int64);
			typeCodesToType[12]	= typeof(UInt64);
			typeCodesToType[13]	= typeof(float);
			typeCodesToType[14]	= typeof(double);
			typeCodesToType[15]	= typeof(decimal);
			typeCodesToType[16]	= typeof(DateTime);
			typeCodesToType[18]	= typeof(string);
		}
		
		#endregion

		#region Methods

		/// <summary>
		/// Determins default value for given type.
		/// </summary>
		/// <returns>Default value for a given type.</returns>
		/// <param name="_type">Type.</param>
		public static object DefaultValue (this Type _type)
		{
			if (_type.IsValueType)
				return Activator.CreateInstance(_type);

			return null;
		}

		public static Type GetTypeFromTypeCode (this TypeCode _typeCode)
		{
			return typeCodesToType[(int)_typeCode];
		}

		#endregion
#else
		#region Methods
		public static ConstructorInfo GetConstructor(Type type, Type[] args)
		{
			foreach (ConstructorInfo current in type.GetTypeInfo().DeclaredConstructors)
			{
				if (TypeExtensions.ParametersMatch(current.GetParameters(), args))
				{
					return current;
				}
			}
			return null;
		}
		
		private static bool ParametersMatch(ParameterInfo[] parameters, Type[] args)
		{
			if (parameters.Length == args.Length)
			{
				bool result = true;
				for (int i = 0; i < parameters.Length; i++)
				{
					Type parameterType = parameters[i].ParameterType;
					Type type = args[i];
					if (parameterType != type)
					{
						result = false;
						break;
					}
				}
				return result;
			}
			return false;
		}
		
		public static Type[] GetGenericArguments(Type type)
		{
			return type.GetTypeInfo().GenericTypeParameters;
		}
		
		public static Type[] GetInterfaces(Type type)
		{
			List<Type> list = new List<Type>();
			foreach (Type current in type.GetTypeInfo().ImplementedInterfaces)
			{
				list.Add(current);
			}
			return list.ToArray();
		}
		
		public static MethodInfo[] GetOpImplicitMethodsFromTypeHierarchy(Type type, System.Reflection.BindingFlags bindingFlags)
		{
			if ((bindingFlags & System.Reflection.BindingFlags.NonPublic) == System.Reflection.BindingFlags.NonPublic)
			{
				throw new NotImplementedException("System.Type.GetMethods(BindingFlags) called with invalid BindingFlags : " + bindingFlags);
			}
			TypeInfo typeInfo = type.GetTypeInfo();
			List<MethodInfo> list = new List<MethodInfo>();
			while (true)
			{
				IEnumerable<MethodInfo> declaredMethods = typeInfo.GetDeclaredMethods("op_Implicit");
				list.AddRange(declaredMethods);
				if (object.ReferenceEquals(typeInfo.BaseType, null))
				{
					break;
				}
				typeInfo = typeInfo.BaseType.GetTypeInfo();
			}
			return list.ToArray();
		}
		
		public static PropertyInfo[] GetPublicInstanceProperties(Type type)
		{
			List<PropertyInfo> list = new List<PropertyInfo>();
			foreach (PropertyInfo current in type.GetRuntimeProperties())
			{
				if (current.GetMethod.IsPublic && !current.GetMethod.IsStatic)
				{
					list.Add(current);
				}
			}
			return list.ToArray();
		}
		
		public static bool ReferenceNotEquals(object lhs, object rhs)
		{
			return !object.ReferenceEquals(lhs, rhs);
		}
		
		public static bool IsValueType(Type type)
		{
			return type.GetTypeInfo().IsValueType;
		}
		
		public static bool EnumToBoolean(Enum @enum, object formatProvider)
		{
			return Convert.ToInt32(@enum) != 0;
		}
		
		public static bool IsSubClassOf(Type toBeChecked, Type type)
		{
			return toBeChecked.GetTypeInfo().IsSubclassOf(type);
		}
		
		public static bool IsInstanceOfType(Type _this, object obj)
		{
			return _this != null && obj != null && _this.GetTypeInfo().IsAssignableFrom(obj.GetType().GetTypeInfo());
		}
		
		public static System.TypeCode GetTypeCode(Type type)
		{
			if (object.ReferenceEquals(type, null))
			{
				return System.TypeCode.Empty;
			}
			if (object.ReferenceEquals(type, typeof(object)))
			{
				return System.TypeCode.Object;
			}
			if (object.ReferenceEquals(type, typeof(byte)))
			{
				return System.TypeCode.Byte;
			}
			if (object.ReferenceEquals(type, typeof(short)))
			{
				return System.TypeCode.Int16;
			}
			if (object.ReferenceEquals(type, typeof(int)))
			{
				return System.TypeCode.Int32;
			}
			if (object.ReferenceEquals(type, typeof(long)))
			{
				return System.TypeCode.Int64;
			}
			if (object.ReferenceEquals(type, typeof(bool)))
			{
				return System.TypeCode.Boolean;
			}
			if (object.ReferenceEquals(type, typeof(char)))
			{
				return System.TypeCode.Char;
			}
			if (object.ReferenceEquals(type, typeof(string)))
			{
				return System.TypeCode.String;
			}
			if (object.ReferenceEquals(type, typeof(float)))
			{
				return System.TypeCode.Single;
			}
			if (object.ReferenceEquals(type, typeof(double)))
			{
				return System.TypeCode.Double;
			}
			if (object.ReferenceEquals(type, typeof(DateTime)))
			{
				return System.TypeCode.DateTime;
			}
			if (object.ReferenceEquals(type, typeof(decimal)))
			{
				return System.TypeCode.Decimal;
			}
			if (object.ReferenceEquals(type, typeof(sbyte)))
			{
				return System.TypeCode.SByte;
			}
			if (object.ReferenceEquals(type, typeof(ushort)))
			{
				return System.TypeCode.UInt16;
			}
			if (object.ReferenceEquals(type, typeof(uint)))
			{
				return System.TypeCode.UInt32;
			}
			if (object.ReferenceEquals(type, typeof(ulong)))
			{
				return System.TypeCode.UInt64;
			}
			return System.TypeCode.Object;
		}
		
		internal static bool MatchBindingFlags(bool isPublic, bool isPrivate, bool isStatic, System.Reflection.BindingFlags bindingAttr, bool baseClass)
		{
			if (isPublic)
			{
				if ((bindingAttr & System.Reflection.BindingFlags.Public) == System.Reflection.BindingFlags.Default)
				{
					return false;
				}
			}
			else if ((bindingAttr & System.Reflection.BindingFlags.NonPublic) == System.Reflection.BindingFlags.Default)
			{
				return false;
			}
			if (isStatic)
			{
				if ((bindingAttr & System.Reflection.BindingFlags.Static) == System.Reflection.BindingFlags.Default)
				{
					return false;
				}
				if (baseClass)
				{
					if ((bindingAttr & System.Reflection.BindingFlags.FlattenHierarchy) == System.Reflection.BindingFlags.Default)
					{
						return false;
					}
					if (isPrivate)
					{
						return false;
					}
				}
			}
			else if ((bindingAttr & System.Reflection.BindingFlags.Instance) == System.Reflection.BindingFlags.Default)
			{
				return false;
			}
			return true;
		}
		
		public static MethodInfo GetMethod(this Type type, string name, System.Reflection.BindingFlags bindingAttr)
		{
			return type.GetMethod(name, bindingAttr, false);
		}
		
		private static MethodInfo GetMethod(this Type type, string name, System.Reflection.BindingFlags bindingAttr, bool baseClass)
		{
			if (name == null)
			{
				throw new ArgumentNullException("name can not be null");
			}
			MethodInfo methodInfo = null;
			foreach (MethodInfo current in type.GetRuntimeMethods())
			{
				if ((bindingAttr & System.Reflection.BindingFlags.IgnoreCase) != System.Reflection.BindingFlags.Default)
				{
					if (current.Name.ToLower() != name.ToLower())
					{
						continue;
					}
				}
				else if (current.Name != name)
				{
					continue;
				}
				if (TypeExtensions.MatchBindingFlags(current.IsPublic, current.IsPrivate, current.IsStatic, bindingAttr, baseClass))
				{
					if (methodInfo != null)
					{
						throw new AmbiguousMatchException(string.Format("More than one method {0} match the query", new object[]
						                                                {
							name
						}));
					}
					methodInfo = current;
				}
			}
			if ((bindingAttr & System.Reflection.BindingFlags.DeclaredOnly) != System.Reflection.BindingFlags.Default)
			{
				return methodInfo;
			}
			Type baseType = type.GetTypeInfo().BaseType;
			if (baseType != null && baseType != type)
			{
				MethodInfo method = baseType.GetMethod(name, bindingAttr, true);
				if (methodInfo == null)
				{
					methodInfo = method;
				}
				else if (method != null && methodInfo.DeclaringType != method.DeclaringType)
				{
					throw new AmbiguousMatchException(string.Format("More than one method {0} match the query", new object[]
					                                                {
						name
					}));
				}
				return methodInfo;
			}
			return methodInfo;
		}
		
		public static PropertyInfo GetProperty(this Type type, string name, System.Reflection.BindingFlags bindingAttr)
		{
			return type.GetProperty(name, bindingAttr, false);
		}
		
		private static PropertyInfo GetProperty(this Type type, string name, System.Reflection.BindingFlags bindingAttr, bool baseClass)
		{
			if (name == null)
			{
				throw new ArgumentNullException("name can not be null");
			}
			PropertyInfo propertyInfo = null;
			foreach (PropertyInfo current in type.GetRuntimeProperties())
			{
				if ((bindingAttr & System.Reflection.BindingFlags.IgnoreCase) != System.Reflection.BindingFlags.Default)
				{
					if (current.Name.ToLower() != name.ToLower())
					{
						continue;
					}
				}
				else if (current.Name != name)
				{
					continue;
				}
				MethodInfo methodInfo = current.GetMethod;
				if (methodInfo == null)
				{
					methodInfo = current.SetMethod;
				}
				if (TypeExtensions.MatchBindingFlags(methodInfo.IsPublic, methodInfo.IsPrivate, methodInfo.IsStatic, bindingAttr, baseClass))
				{
					if (propertyInfo != null)
					{
						throw new AmbiguousMatchException(string.Format("More than one property {0} match the query", new object[]
						                                                {
							name
						}));
					}
					propertyInfo = current;
				}
			}
			if ((bindingAttr & System.Reflection.BindingFlags.DeclaredOnly) != System.Reflection.BindingFlags.Default)
			{
				return propertyInfo;
			}
			Type baseType = type.GetTypeInfo().BaseType;
			if (baseType != null && baseType != type)
			{
				PropertyInfo property = baseType.GetProperty(name, bindingAttr, true);
				if (propertyInfo == null)
				{
					propertyInfo = property;
				}
				else if (property != null && propertyInfo.DeclaringType != property.DeclaringType)
				{
					throw new AmbiguousMatchException(string.Format("More than one property {0} match the query", new object[]
					                                                {
						name
					}));
				}
				return propertyInfo;
			}
			return propertyInfo;
		}
		
		public static FieldInfo GetField(this Type type, string name, System.Reflection.BindingFlags bindingAttr)
		{
			return type.GetField(name, bindingAttr, false);
		}
		
		private static FieldInfo GetField(this Type type, string name, System.Reflection.BindingFlags bindingAttr, bool baseClass)
		{
			if (name == null)
			{
				throw new ArgumentNullException("name can not be null");
			}
			FieldInfo fieldInfo = null;
			foreach (FieldInfo current in type.GetRuntimeFields())
			{
				if ((bindingAttr & System.Reflection.BindingFlags.IgnoreCase) != System.Reflection.BindingFlags.Default)
				{
					if (current.Name.ToLower() != name.ToLower())
					{
						continue;
					}
				}
				else if (current.Name != name)
				{
					continue;
				}
				if (TypeExtensions.MatchBindingFlags(current.IsPublic, current.IsPrivate, current.IsStatic, bindingAttr, baseClass))
				{
					if (fieldInfo != null)
					{
						throw new AmbiguousMatchException(string.Format("More than one field {0} match the query", new object[]
						                                                {
							name
						}));
					}
					fieldInfo = current;
				}
			}
			if ((bindingAttr & System.Reflection.BindingFlags.DeclaredOnly) != System.Reflection.BindingFlags.Default)
			{
				return fieldInfo;
			}
			Type baseType = type.GetTypeInfo().BaseType;
			if (baseType != null && baseType != type)
			{
				FieldInfo field = baseType.GetField(name, bindingAttr, true);
				if (fieldInfo == null)
				{
					fieldInfo = field;
				}
				else if (field != null && fieldInfo.DeclaringType != field.DeclaringType)
				{
					throw new AmbiguousMatchException(string.Format("More than one field {0} match the query", new object[]
					                                                {
						name
					}));
				}
				return fieldInfo;
			}
			return fieldInfo;
		}
		
		public static Type BaseType(this Type type)
		{
			Type baseType = type.GetTypeInfo().BaseType;
			return baseType;
		}
		#endregion 
#endif
	}
}
