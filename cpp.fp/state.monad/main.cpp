#include <iostream>
#include <functional>
#include <concepts>
#include <utility>

// State 모나드 클래스 정의
template<typename S, typename A>
class State {
public:
    using StateFunc = std::function<std::pair<A, S>(S)>;

private:
    StateFunc runState;

public:
    explicit State(StateFunc runStateFunc) : runState(std::move(runStateFunc)) {}

    std::pair<A, S> run(S state) const {
        return runState(state);
    }

    // bind 함수를 사용해 모나드 연산을 연결
    template<typename B>
    State<S, B> bind(std::invocable<A> auto f) const {
        return State<S, B>([this, f](S state) {
            auto [a, newState] = this->run(state);  // 현재 모나드 실행
            return f(a).run(newState);              // 새로운 모나드 실행
        });
    }

    // 상태를 전달받지 않는 pure 함수
    static State<S, A> pure(A value) {
        return State<S, A>([value](S state) {
            return std::make_pair(value, state);
        });
    }
};

// 상태를 가져오는 get 함수
template<typename S>
State<S, S> get() {
    return State<S, S>([](S state) {
        return std::make_pair(state, state);
    });
}

// 상태를 설정하는 put 함수
template<typename S>
State<S, void> put(S newState) {
    return State<S, void>([newState](S) {
        return std::make_pair(nullptr, newState);
    });
}

// 카운터를 증가시키는 예제
State<int, int> increment() {
    return get<int>().bind<int>([](int state) {
        return put<int>(state + 1).bind<int>([state](...) {
            return State<int, int>::pure(state);
        });
    });
}

int main() {
    // 상태 모나드와 바인딩 연산을 사용한 상태 처리 예제
    State<int, int> program = increment().bind<int>([](int prevState) {
        std::cout << "Previous state: " << prevState << std::endl;
        return increment();  // 두 번째 increment 호출
    });

    auto [result, finalState] = program.run(0);  // 초기 상태는 0

    std::cout << "Final result: " << result << std::endl;
    std::cout << "Final state: " << finalState << std::endl;

    return 0;
}