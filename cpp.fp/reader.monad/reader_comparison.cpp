#include <iostream>
#include <functional>
#include <string>
#include <memory>
#include <chrono>

using namespace std;
using namespace std::chrono;

////////////////////////////////////////////////////////////////////////////////
//
// 실제 문제: 여러 서비스가 설정을 공유하며 동작
//
////////////////////////////////////////////////////////////////////////////////

struct AppConfig {
    string db_host;
    int db_port;
    string api_key;
    int timeout_ms;
    bool debug_mode;
};

////////////////////////////////////////////////////////////////////////////////
//
// 방법 1: 전통적인 C++ - 생성자 주입
//
////////////////////////////////////////////////////////////////////////////////

class TraditionalService {
private:
    const AppConfig& config_;
    
public:
    explicit TraditionalService(const AppConfig& config) : config_(config) {}
    
    string fetchUserData(int userId) {
        if (config_.debug_mode) {
            cout << "[DEBUG] Fetching user " << userId << " from " 
                 << config_.db_host << ":" << config_.db_port << "\n";
        }
        return "User_" + to_string(userId);
    }
    
    string processData(const string& data) {
        if (config_.debug_mode) {
            cout << "[DEBUG] Processing with timeout " << config_.timeout_ms << "ms\n";
        }
        return "Processed: " + data;
    }
};

void traditionalApproach() {
    cout << "=== Traditional Approach (Constructor Injection) ===\n";
    
    AppConfig config{"localhost", 5432, "secret", 3000, true};
    
    // 명시적이고 직관적
    TraditionalService service(config);
    auto userData = service.fetchUserData(123);
    auto result = service.processData(userData);
    cout << "Result: " << result << "\n\n";
    
    // 장점:
    // - 간단하고 명확
    // - 디버깅 쉬움
    // - 성능 오버헤드 없음
    // - 모든 C++ 개발자가 이해
}

////////////////////////////////////////////////////////////////////////////////
//
// 방법 2: Reader Monad
//
////////////////////////////////////////////////////////////////////////////////

template <typename Env, typename T>
class Reader {
    function<T(Env)> computation;
public:
    explicit Reader(function<T(Env)> f) : computation(f) {}
    
    T run(Env env) const { return computation(env); }
    
    template <typename F>
    auto map(F f) const {
        return Reader<Env, decltype(f(declval<T>()))>(
            [*this, f](Env env) { return f(this->run(env)); }
        );
    }
    
    template <typename F>
    auto flatMap(F f) const {
        using ResultReader = decltype(f(declval<T>()));
        using ResultType = decltype(declval<ResultReader>().run(declval<Env>()));
        return Reader<Env, ResultType>(
            [*this, f](Env env) { return f(this->run(env)).run(env); }
        );
    }
};

// Reader Monad 스타일 함수들
Reader<AppConfig, string> fetchUserDataReader(int userId) {
    return Reader<AppConfig, string>([userId](AppConfig config) {
        if (config.debug_mode) {
            cout << "[DEBUG] Fetching user " << userId << " from " 
                 << config.db_host << ":" << config.db_port << "\n";
        }
        return "User_" + to_string(userId);
    });
}

Reader<AppConfig, string> processDataReader(const string& data) {
    return Reader<AppConfig, string>([data](AppConfig config) {
        if (config.debug_mode) {
            cout << "[DEBUG] Processing with timeout " << config.timeout_ms << "ms\n";
        }
        return "Processed: " + data;
    });
}

void readerMonadApproach() {
    cout << "=== Reader Monad Approach ===\n";
    
    AppConfig config{"localhost", 5432, "secret", 3000, true};
    
    // 복잡한 합성
    auto computation = fetchUserDataReader(123)
        .flatMap([](const string& userData) {
            return processDataReader(userData);
        });
    
    auto result = computation.run(config);
    cout << "Result: " << result << "\n\n";
    
    // 단점:
    // - 복잡한 문법
    // - 성능 오버헤드 (std::function)
    // - 디버깅 어려움
    // - 팀원 교육 필요
}

////////////////////////////////////////////////////////////////////////////////
//
// 방법 3: 하이브리드 - 실용적 중간 지점
//
////////////////////////////////////////////////////////////////////////////////

// Config를 전역 컨텍스트로 관리하되, 명시적으로 전달
class ConfigContext {
private:
    const AppConfig* config_ = nullptr;
    
public:
    class Scope {
        ConfigContext& ctx_;
        const AppConfig* prev_;
    public:
        Scope(ConfigContext& ctx, const AppConfig& config) 
            : ctx_(ctx), prev_(ctx.config_) {
            ctx_.config_ = &config;
        }
        ~Scope() { ctx_.config_ = prev_; }
    };
    
    const AppConfig& get() const { 
        if (!config_) throw runtime_error("No config set");
        return *config_; 
    }
    
    static ConfigContext& instance() {
        static ConfigContext ctx;
        return ctx;
    }
};

// 함수들이 암묵적으로 context 사용
string fetchUserDataHybrid(int userId) {
    auto& config = ConfigContext::instance().get();
    if (config.debug_mode) {
        cout << "[DEBUG] Fetching user " << userId << " from " 
             << config.db_host << ":" << config.db_port << "\n";
    }
    return "User_" + to_string(userId);
}

void hybridApproach() {
    cout << "=== Hybrid Approach ===\n";
    
    AppConfig config{"localhost", 5432, "secret", 3000, true};
    
    // RAII 스타일로 컨텍스트 설정
    ConfigContext::Scope scope(ConfigContext::instance(), config);
    
    // 간단한 함수 호출
    auto userData = fetchUserDataHybrid(123);
    cout << "Result: " << userData << "\n\n";
}

////////////////////////////////////////////////////////////////////////////////
//
// 성능 비교
//
////////////////////////////////////////////////////////////////////////////////

void performanceComparison() {
    cout << "=== Performance Comparison ===\n";
    
    const int iterations = 1'000'000;
    AppConfig config{"localhost", 5432, "secret", 3000, false};
    
    // Traditional
    auto start = high_resolution_clock::now();
    {
        TraditionalService service(config);
        for (int i = 0; i < iterations; i++) {
            volatile auto result = service.fetchUserData(i);
        }
    }
    auto traditional_time = duration_cast<milliseconds>(
        high_resolution_clock::now() - start).count();
    
    // Reader Monad
    start = high_resolution_clock::now();
    {
        for (int i = 0; i < iterations; i++) {
            volatile auto result = fetchUserDataReader(i).run(config);
        }
    }
    auto reader_time = duration_cast<milliseconds>(
        high_resolution_clock::now() - start).count();
    
    cout << "Traditional: " << traditional_time << "ms\n";
    cout << "Reader Monad: " << reader_time << "ms\n";
    cout << "Overhead: " << (reader_time / (double)traditional_time) << "x\n\n";
}

////////////////////////////////////////////////////////////////////////////////
//
// 언제 Reader Monad가 적절한가?
//
////////////////////////////////////////////////////////////////////////////////

void whenToUseReaderMonad() {
    cout << "=== When is Reader Monad Appropriate? ===\n\n";
    
    cout << "❌ AVOID Reader Monad when:\n";
    cout << "  • Working in a team unfamiliar with FP\n";
    cout << "  • Performance is critical\n";
    cout << "  • Simple dependency injection suffices\n";
    cout << "  • Config rarely changes during execution\n\n";
    
    cout << "✅ CONSIDER Reader Monad when:\n";
    cout << "  • Building a DSL or interpreter\n";
    cout << "  • Need to swap environments dynamically\n";
    cout << "  • Teaching/learning FP concepts\n";
    cout << "  • Building a purely functional library\n\n";
    
    cout << "🎯 PRACTICAL ALTERNATIVES:\n";
    cout << "  1. Constructor injection (most cases)\n";
    cout << "  2. Factory pattern with config\n";
    cout << "  3. Context objects (like OpenGL)\n";
    cout << "  4. Template parameters for compile-time config\n\n";
}

////////////////////////////////////////////////////////////////////////////////
//
// Main
//
////////////////////////////////////////////////////////////////////////////////

int main() {
    cout << "=== Is Reader Monad Over-Engineering in C++? ===\n\n";
    
    traditionalApproach();
    readerMonadApproach();
    hybridApproach();
    performanceComparison();
    whenToUseReaderMonad();
    
    cout << "=== VERDICT ===\n";
    cout << "Reader Monad in C++ is USUALLY over-engineering because:\n";
    cout << "• " << "50x+ performance overhead\n";
    cout << "• Complex syntax without language support\n";
    cout << "• Traditional DI patterns work well\n";
    cout << "• Team collaboration challenges\n\n";
    
    cout << "Use simple constructor injection instead.\n";
    cout << "Save Reader Monad for educational purposes or\n";
    cout << "very specific functional programming libraries.\n";
    
    return 0;
}