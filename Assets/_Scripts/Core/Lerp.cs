using UnityEngine;

using System;

namespace Nanome.Maths
{

    public abstract class Lerp<T>
    {

        private static float minimumLerpTime = 0.00001f;

        private float startTime;
        private float totalTime;

        protected T startValue;
        protected T endValue;

        private bool markedDone = false;

        public Lerp(float seconds = 1f)
        {
            reset(seconds);
        }

        public void reset(float seconds)
        {
            startTime = now();
            totalTime = seconds;
            if (totalTime <= minimumLerpTime)
            {
                totalTime = minimumLerpTime;
            }
        }

        public float ratio()
        {
            var r = (now() - startTime) / totalTime;
            return Mathf.Min(Mathf.Max(r, 0f), 1f);
        }

        public bool done()
        {
            var shouldBeDone = (now() > (startTime + totalTime));
            if (!shouldBeDone)
            {
                return false;
            }
            if (!markedDone)
            {
                markedDone = true;
                return false;
            }
            return true;
        }

        public void init(T start, T end, float seconds = 1f)
        {
            reset(seconds);
            startValue = start;
            endValue = end;
        }

        private float now()
        {
            return Time.time;
        }

        abstract public T current();

    }

    public class LerpVector3 : Lerp<Vector3>
    {

        public LerpVector3(Vector3 start, Vector3 end, float seconds = 1f) : base(seconds)
        {
            init(start, end, seconds);
        }

        public override Vector3 current()
        {
            return Vector3.Lerp(startValue, endValue, ratio());
        }

        public Vector3 currentSlerp()
        {
            return Vector3.Slerp(startValue, endValue, ratio());
        }

    }

    public class LerpQuaternion : Lerp<Quaternion>
    {

        public LerpQuaternion(Quaternion start, Quaternion end, float seconds = 1f) : base(seconds)
        {
            init(start, end, seconds);
        }

        public override Quaternion current()
        {
            return Quaternion.Lerp(startValue, endValue, ratio());
        }

        public Quaternion currentSlerp()
        {
            return Quaternion.Slerp(startValue, endValue, ratio());
        }

    }

    public class LerpFloat : Lerp<float>
    {

        public LerpFloat(float start, float end, float seconds = 1f) : base(seconds)
        {
            init(start, end, seconds);
        }

        public override float current()
        {
            var r = ratio();
            return startValue * (1f - r) + endValue * r;
        }

    }

    public class LerpColor : Lerp<Color>
    {

        public LerpColor(Color start, Color end, float seconds = 1f) : base(seconds)
        {
            init(start, end, seconds);
        }

        public override Color current()
        {
            return Color.Lerp(startValue, endValue, ratio());
        }

    }

}